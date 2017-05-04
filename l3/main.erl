-module(main).
-export([start/0, play/1, stop/0, ping_loop/0, pong_loop/0]).

start() -> 
	register(ping, spawn(main, ping_loop, [])),
	register(pong, spawn(main, pong_loop, [])).

stop() ->
  ping ! stop,
  pong ! stop.

play(N) ->
  ping ! N.

ping_loop() ->
	receive
    stop -> ok;
    0 -> ping_loop();
		N -> pong ! N-1, io:format("ping~n"), ping_loop()
	after
    20000 -> ok2
  end.


pong_loop() ->
  receive
    stop -> ok;
    0 -> pong_loop();
    N -> ping ! N-1, io:format("pong~n"), pong_loop()
  after
    20000 -> ok2
  end.