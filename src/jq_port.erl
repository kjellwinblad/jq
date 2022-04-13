-module(jq_port).
-export([start/1, stop/0, init/1, start_recording/1, stop_recording/0]).
-export([jq_process_json/2]).

start(ExtPrg) ->
    spawn(?MODULE, init, [ExtPrg]).

stop() ->
    case lists:any(fun(X) -> X =:= complex end, erlang:registered()) of
        false -> 
            ok;
        true ->
            complex ! stop
    end,
    ok.

start_recording(FileName) ->
    complex ! {start_recording,
               erlang:iolist_to_binary([FileName, "\0"])},
    ok.

stop_recording() ->
    complex ! stop_recording,
    ok.

jq_process_json(FilterProgram, JSONText) ->
    call_port({jq_process_json, FilterProgram, JSONText}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
	{complex, Result} ->
	    Result
    end.

init(ExtPrg) ->
    case lists:any(fun(X) -> X =:= complex end, erlang:registered()) of
        false -> 
            erlang:register(complex, self()),
            process_flag(trap_exit, true),
            Port = erlang:open_port({spawn, "./priv/erlang_jq_port"}, [{packet, 4}, binary]),
            loop(Port);
        true ->
            ok
    end,
    ok.

receive_result_blobs(_, 0, SoFar) ->
    lists:reverse(SoFar);
receive_result_blobs(Port, Count, SoFar) ->
    receive
        {Port, {data, Data}} ->
            receive_result_blobs(Port, Count - 1, [erlang:iolist_to_binary(Data) | SoFar])
    end.

loop(Port) ->
    receive
	{call, Caller, {jq_process_json, FilterProgram, JSONText}} ->
	    Port ! {self(), {command, <<"process_json\0">>}},
	    Port ! {self(), {command, [FilterProgram, 0]}},
	    Port ! {self(), {command, [JSONText, 0]}},
	    receive
		{Port, {data, DataType}} ->
                    case erlang:iolist_to_binary(DataType) of
                        <<"ok">> ->
                            receive
                                {Port, {data, SizeStr}} ->
                                    NrOfItems = erlang:binary_to_integer(SizeStr),
                                    Caller ! {complex, {ok, receive_result_blobs(Port, NrOfItems, [])}}
                            end;
                        <<"error">> ->
                            [ErrorType, ErrorMsg] = receive_result_blobs(Port, 2, []),
                            ErrorTypeBin = erlang:iolist_to_binary(ErrorType),
                            ErrorMsgBin = erlang:iolist_to_binary(ErrorMsg),
                            Caller ! {complex, {error, {erlang:binary_to_atom(ErrorTypeBin), ErrorMsgBin}}}
                    end
	    end,
	    loop(Port);
	stop ->
            erlang:display(sending_exit),
            Port ! {self(), {command, <<"exit\0">>}},
            erlang:display(waiting_for_exit),
            receive
                {Port, {data, <<"exiting">>}} ->
                    erlang:display(exited)
            end;
        {start_recording, FileName} ->
	    Port ! {self(), {command, <<"start_record_input\0">>}},
	    Port ! {self(), {command, FileName}},
	    loop(Port);
        stop_recording ->
	    Port ! {self(), {command, <<"stop_record_input\0">>}},
	    loop(Port);
        % ,
	    % receive
		% {Port, closed} ->
		    % exit(normal)
	    % end;
	{'EXIT', Port, Reason} ->
	    exit(port_terminated)
    end.

encode({bar, Y}) -> [4, Y].

decode([Int]) -> Int.
