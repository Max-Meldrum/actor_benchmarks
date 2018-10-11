-module(pingpong).

-export([start/1, ping/3, pong/0]).

ping(0, Pong_PID, MASTER) ->
    MASTER ! finished;

ping(N, Pong_PID, MASTER) ->
    Pong_PID ! {ping, self()},
    receive
        pong -> ok 
    end,
    ping(N - 1, Pong_PID, MASTER).

pong() ->
    receive
        {ping, Ping_PID} ->
            Ping_PID ! pong,
            pong()
    end.

start(N) ->
    Start=now(),
    Pong_PID = spawn(pingpong, pong, []),
    spawn(pingpong, ping, [N, Pong_PID, self()]),
    receive
        finished ->
            Finish=now(),
            io:format("Test took ~p seconds~n",[elapsedTime(Start,Finish)])
    end.

elapsedTime(Start,Finish) -> 
        (toMicroSeconds(Finish) - toMicroSeconds(Start)) /1000000.

toMicroSeconds({MegaSeconds,Seconds,MicroSeconds}) -> 
            (MegaSeconds+Seconds) * 1000000 + MicroSeconds.
        
