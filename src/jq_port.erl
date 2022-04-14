-module(jq_port).

-export([start/0, stop/0, init/2, start_recording/1, stop_recording/0, set_filter_program_lru_cache_max_size/1, get_filter_program_lru_cache_max_size/0]).
-export([jq_process_json/2]).

start() ->
    Ref = erlang:make_ref(),
    spawn(?MODULE, init, [Ref, self()]),
    receive
        Ref -> ok
    end.

stop() ->
    case lists:any(fun(X) -> X =:= complex end, erlang:registered()) of
        false -> 
            ok;
        true ->
            call_port(stop)
    end,
    ok.

start_recording(FileName) ->
    complex ! {start_recording,
               erlang:iolist_to_binary([FileName, "\0"])},
    ok.

stop_recording() ->
    complex ! stop_recording,
    ok.

get_filter_program_lru_cache_max_size() ->
    call_port(get_filter_program_lru_cache_max_size).

set_filter_program_lru_cache_max_size(NewSize) when NewSize >= 0, NewSize < 1073741824 ->
    complex ! {set_filter_program_lru_cache_max_size, NewSize},
    ok.

jq_process_json(FilterProgram, JSONText) ->
    call_port({jq_process_json, FilterProgram, JSONText}).

call_port(Msg) ->
    complex ! {call, self(), Msg},
    receive
	{complex, Result} ->
	    Result
    end.

init(Ref, Parent) ->
    case lists:any(fun(X) -> X =:= complex end, erlang:registered()) of
        false -> 
            erlang:register(complex, self()),
            process_flag(trap_exit, true),
            Port = erlang:open_port({spawn, "./priv/erlang_jq_port"}, [{packet, 4}, binary]),
            Parent ! Ref,
            loop(Port);
        true ->
            Parent ! Ref,
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
	{call, Caller, stop} ->
            Port ! {self(), {command, <<"exit\0">>}},
            receive
                {Port, {data, <<"exiting">>}} ->
                    Caller ! {complex, ok}
            end;
	{call, Caller, get_filter_program_lru_cache_max_size} ->
            Port ! {self(), {command, <<"get_filter_program_lru_cache_max_size\0">>}},
            receive
                {Port, {data, CacheSizeStr}} ->
                    Caller ! {complex, erlang:binary_to_integer(CacheSizeStr)}
            end,
            loop(Port);
        {set_filter_program_lru_cache_max_size, NewSize} ->
            Port ! {self(), {command, <<"set_filter_program_lru_cache_max_size\0">>}},
            Port ! {self(), {command, erlang:iolist_to_binary([erlang:integer_to_list(NewSize), "\0"])}},
            loop(Port);
        {start_recording, FileName} ->
	    Port ! {self(), {command, <<"start_record_input\0">>}},
	    Port ! {self(), {command, FileName}},
	    loop(Port);
        stop_recording ->
	    Port ! {self(), {command, <<"stop_record_input\0">>}},
	    loop(Port);
	{'EXIT', Port, Reason} ->
	    exit(port_terminated);
        Other ->
            erlang:display({some_other_message, Other})
    end.

encode({bar, Y}) -> [4, Y].

decode([Int]) -> Int.
