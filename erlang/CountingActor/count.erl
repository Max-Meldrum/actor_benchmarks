-module(count).

-export([start/1, counters/1, producer/3]).

counters(0) ->
    receive
        increment -> ok
        %  {result, producer_PID} -> producer_PID ! {result, N}
    end,
    counters(1);

counters(N) ->
        receive
            % increment -> io:format("HEY!");
            {result, Producer_PID} -> Producer_PID ! {result, N}
        end,
        counters(N + 1).

producer(0, C_PID, MASTER) ->
    C_PID ! {result, self()},
    receive
        
        {result, COUNT} -> io:format("BYE!"),
            MASTER ! finished
    end;

producer(N, C_PID, MASTER) ->
    C_PID ! increment,
    producer(N - 1, C_PID, MASTER).


start(N) ->
    Start=now(),
    Counter_PID = spawn(count, counters, [0]),
    spawn(count, producer, [N, Counter_PID, self()]),

    receive
        finished ->
            Finish=now(),
            io:format("Test took ~p seconds~n",[elapsedTime(Start,Finish)])
    end.

elapsedTime(Start,Finish) -> 
        (toMicroSeconds(Finish) - toMicroSeconds(Start)) /1000000.

toMicroSeconds({MegaSeconds,Seconds,MicroSeconds}) -> 
            (MegaSeconds+Seconds) * 1000000 + MicroSeconds.
        
