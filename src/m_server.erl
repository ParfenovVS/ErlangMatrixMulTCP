%%%-------------------------------------------------------------------
%%% @author Vladimir
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. ����. 2015 13:54
%%%-------------------------------------------------------------------
-module(m_server).
-author("Vladimir").

%% API

-export([generate/6, start/0, run/2, run/4, runAsync/2, async/4, mulVProc/4, toMatrix/1, print/1]).

-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).

transformData([R1, C1, R2, C2 | Data]) ->
  M1 = generate(1, R1, C1, Data, 0, []),
  M2 = generate(1, R2, C2, Data, R1 * C1, []),
  [M1, M2].

generate(R, Rows, _Cols, _Data, _Shift, Result) when R =:= Rows + 1 -> Result;
generate(R, Rows, Cols, Data, Shift, Result) ->
  generate(R + 1, Rows, Cols, Data, Shift, lists:append(Result, [lists:sublist(Data, Shift + (R - 1) * Cols + 1, Cols)])).


start() ->
  {ok, LSocket} = gen_tcp:listen(8080, ?TCP_OPTIONS),
  accept(LSocket).

accept(LSocket) ->
  {ok, Socket} = gen_tcp:accept(LSocket),
  spawn(fun() -> loop(Socket) end),
  accept(LSocket).

loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      run(Socket, transformData(Data)),
      loop(Socket);
    {error, closed} ->
      ok
  end.

rand(Rows, Cols) ->
  [[random:uniform(10) || _ <- lists:seq(1, Cols)] || _ <- lists:seq(1, Rows)].

run(R1, C1, R2, C2) ->
  Begin = erlang:timestamp(),
  M1 = rand(R1, C1),
  M2 = rand(R2, C2),
  mul(M1, M2),
  listen(M1, M2, 0, length(M1) * length(lists:nth(1, M2))),
  End = erlang:timestamp(),
  io:format("Begin = ~w~n", [Begin]),
  io:format("End = ~w~n", [End]),
  Time = round(((element(2, End) * 1000 + element(3, End) / 1000) - (element(2, Begin) * 1000 + element(3, Begin) / 1000))),
  io:format("Time (ms) = ~w~n", [Time]).

run(Socket, [M1, M2]) ->
  Begin = erlang:system_time(),
  process_flag(trap_exit, true),
  io:format("=====M1=====~n"),
  print(M1),
  io:format("=====M2=====~n"),
  print(M2),
  io:format("===Result===~n"),
  mul(M1, M2),
  Result = toMatrix(listen(M1, M2, 0, length(M1) * length(lists:nth(1, M2)))),
  print(Result),
  End = erlang:system_time(),
  io:format("Begin = ~w~n", [Begin]),
  io:format("End = ~w~n", [End]),
  Time = (End - Begin) / 1000,
  io:format("Time = ~w~n", [Time]),
  ok = gen_tcp:send(Socket, Result),
  ok = gen_tcp:close(Socket).

runAsync(Socket, [M1, M2]) ->
  process_flag(trap_exit, true),
  io:format("=====M1=====~n"),
  print(M1),
  io:format("=====M2=====~n"),
  print(M2),
  io:format("===Result===~n"),
  mul(M1, M2),
  Pid = spawn(?MODULE, async, [Socket, M1, M2, length(M1) * length(lists:nth(1, M2))]),
  Pid ! {self(), start},
  ok.

print(M) ->
  print(M, 1).
print(M, N) when N =:= length(M) + 1 -> io:format("~n");
print(M, N) ->
  Cur = lists:nth(N, M),
  lists:foreach(fun(X) -> io:format("~w ", [X]) end, Cur),
  io:format("~n"),
  print(M, N + 1).

toMatrix(Result) ->
  NumRows = length(lists:filter(fun({_, _, Y}) -> Y =:= 1 end, Result)),
  toMatrix(lists:keysort(2, Result), 1, NumRows, []).
toMatrix(_Result, Row, NumRows, DoubleList) when Row =:= NumRows + 1 -> lists:reverse(DoubleList);
toMatrix(Result, Row, NumRows, DoubleList) ->
  R = lists:filter(fun({_, X, _}) -> Row =:= X end, Result),
  SortedR = lists:keysort(3, R),
  toMatrix(Result, Row + 1, NumRows,
    [lists:foldr(fun(X, Acc) -> [element(1, X) | Acc] end, [], SortedR) | DoubleList]).

mul(M1, M2) -> mul(M1, M2, 1, []).
mul(M1, _M2, Row, Result) when Row =:= length(M1) + 1 -> Result;
mul(M1, M2, Row, Result) ->
  mul(M1, M2, Row + 1, [Result | mulVonM(lists:nth(Row, M1), M2, Row)]).

mulV(V1, V2, R, C) ->
%%  Value = lists:sum(lists:zipwith(fun(X, Y) -> X * Y end, V1, V2)).
  Pid = spawn(?MODULE, mulVProc, [V1, V2, R, C]),
  erlang:monitor(process, Pid),
  Pid ! {self(), start},
  Pid.

mulVProc(V1, V2, R, C) ->
  receive
    {From, start} ->
      From ! {started, self(), R, C},
      Value = lists:sum(lists:zipwith(fun(X, Y) -> X * Y end, V1, V2)),
      From ! {scalar, Value, {place, R, C}},
      exit(normal)
  end.

getCol(M, Column) when Column =:= length(hd(M)) + 1 -> [];
getCol(M, Column) ->
  lists:foldr(fun(X, Acc) -> [lists:nth(Column, X) | Acc] end, [], M).

mulVonM(V, M, R) -> mulVonM(V, M, R, 1, []).
mulVonM(_V, M, _R, Column, Result) when Column =:= length(hd(M)) + 1 -> Result;
mulVonM(V, M, R, Column, Result) ->
  mulVonM(V, M, R, Column + 1, [Result | mulV(V, getCol(M, Column), R, Column)]).

async(Socket, M1, M2, Size) ->
  process_flag(trap_exit, true),
  receive
    {_From, start} ->
      ok = gen_tcp:send(Socket, toMatrix(listen(M1, M2, 0, Size)))
  end.

listen(M1, M2, Count, Size) -> listen(M1, M2, Count, Size, [], []).
listen(_M1, _M2, Count, Size, _Proc, Result) when Count =:= Size -> Result;
listen(M1, M2, Count, Size, Proc, Result) ->
  receive
    {scalar, Value, {place, X, Y}} ->
      listen(M1, M2, Count + 1, Size, Proc, [{Value, X, Y} | Result]);
    {started, Pid, X, Y} ->
      listen(M1, M2, Count, Size, [Proc | {Pid, X, Y}], Result);
    {'DOWN', _Ref, process, From, Reason} when Reason =:= normal ->
      listen(M1, M2, Count, Size, lists:keydelete(From, 1, [Proc]), Result);
    {'DOWN', _Ref, process, From, _Reason} ->
      Place = lists:keysearch(From, 1, Proc),
      X = element(2, Place),
      Y = element(3, Place),
      io:format("Process is down ~w for Row = ~w, Column = ~w~n", [From, X, Y]),
      New = mulV(lists:nth(X, M1), getCol(M2, Y), X, Y),
      listen(M1, M2, Count, Size, lists:keyreplace(From, 1, Proc, {New, X, Y}), Result)
  end.

