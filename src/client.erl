%%%-------------------------------------------------------------------
%%% @author vladimir
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Нояб. 2015 15:57
%%%-------------------------------------------------------------------
-module(client).
-author("vladimir").

%% API
-import(m_server, [print/1, generate/6]).
-export([run/4, runAsync/4, wait/3]).

rand(Rows, Cols) ->
  [[random:uniform(10) || _ <- lists:seq(1, Cols)] || _ <- lists:seq(1, Rows)].

run(R1, C1, R2, C2) when C1 =:= R2 ->
  process_flag(trap_exit, true),
  M1 = rand(R1, C1),
  M2 = rand(R2, C2),
  io:format("=====M1=====~n"),
  print(M1),
  io:format("=====M2=====~n"),
  print(M2),
  {ok, Socket} = gen_tcp:connect("localhost", 8080, [list, {active, false}, {packet, 0}]),
  ok = gen_tcp:send(Socket, [R1, C1, R2, C2, M1 | M2]),
  wait(R1, C2, Socket);
run(_R1, _C1, _R2, _C2) ->
  io:format("~nSize error~n").

runAsync(R1, C1, R2, C2) when C1 =:= R2 ->
  process_flag(trap_exit, true),
  M1 = rand(R1, C1),
  M2 = rand(R2, C2),
  io:format("=====M1=====~n"),
  print(M1),
  io:format("=====M2=====~n"),
  print(M2),
  {ok, Socket} = gen_tcp:connect("localhost", 8080, [list, {active, false}, {packet, 0}]),
  spawn(?MODULE, wait, [R1, C2, Socket]),
  ok = gen_tcp:send(Socket, [R1, C1, R2, C2, M1 | M2]);
runAsync(_R1, _C1, _R2, _C2) ->
  io:format("~nSize error~n").

wait(Rows, Cols, Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      io:format("===Result===~n"),
      print(generate(1, Rows, Cols, Data, 0, [])),
      ok = gen_tcp:close(Socket);
    {error, closed} ->
      error;
    {_,_} ->
      wait(Rows,Cols,Socket)
  end.
