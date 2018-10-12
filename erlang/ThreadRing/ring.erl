-module (ring).
-export ([start/2]).

start(N, M) ->
	statistics(runtime),
    statistics(wall_clock),
		Main_process = self(),
		io:format("Creating ~p ring processes~n", [N]),
		spawn(fun() -> ring(1, N, M, self(), Main_process) end),
		receive
			ended -> void
		end,
		{_, Time1} = statistics(runtime),
		{_, Time2} = statistics(wall_clock),
		U1 = Time1,
		U2 = Time2,
    io:format("Ring benchmark for ~p processes and ~p messages = ~p milliseconds~n", [N, M, U1]).

ring(_, N, _, _, _) when(N =< 0)->
	io:format("Empty ring~n"),
	erlang:error(emptyRing);
ring(_, _, M, _, _) when(M =< 0)->
	io:format("No messages to send~n"),
	erlang:error(noMessagesToSend);
ring(N, N, M, First_process, Main_process) ->
	% io:format("Ring process ~p created~n", [N]),
	% io:format("Sending ~p messages through the ring~n", [M]),
	First_process ! {send, Main_process, Main_process, M},
	loop(M, N, N, First_process, Main_process);
ring(I, N, M, First_process, Main_process) ->
	%  io:format("Ring process ~p created~n", [I]),
	 Next_process = spawn(fun() -> ring(I+1, N, M, First_process, Main_process) end),
	loop(M, I, N, Next_process, Main_process).


loop(M, I, N, Next_process, Main_process) ->
	receive
		{send, From, From_process, TOKEN} ->
			% io:format("Process ~p received message ~p from process ~p ~n", [I, TOKEN, From]),
			if
				TOKEN == 0 ->
					Main_process ! ended;
				true ->
					Next_process ! {send, I, self(), TOKEN-1}
					
			end,
			loop(M, I, N, Next_process, Main_process)
    end.
    
%http://www.rodenas.org/blog/2007/08/27/erlang-ring-problem/