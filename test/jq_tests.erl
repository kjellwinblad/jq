-module(jq_tests).

-include_lib("eunit/include/eunit.hrl").

wrap_setup_cleanup(TestCases) ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     TestCases}.

% change_get_cache_size_t() ->
%     [ ?_assertMatch(ok, jq:set_filter_program_lru_cache_max_size(42)),
%       ?_assertMatch(42, jq:get_filter_program_lru_cache_max_size())
%     ].
% change_get_cache_size_test_() -> wrap_setup_cleanup(change_get_cache_size_t()).

empty_input_t_() ->
    [
     ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<" ">>))
    , ?_assertMatch({ok,[<<"{}">>]}, jq:parse(<<"">>, <<"{}">>))
    , ?_assertMatch({ok,[<<"{}">>]}, jq:parse(<<" ">>, <<"{}">>))
    ].
empty_input_test_() -> wrap_setup_cleanup(empty_input_t_()).

parse_error_t_() ->
    [ ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"{\"b\": }">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"{\"b\"- 2}">>))
    , ?_assertMatch({error, {jq_err_parse, _}}, jq:parse(<<".">>, <<"{\"b\"- 2}">>))
    ].
parse_error_test_() -> wrap_setup_cleanup(parse_error_t_()).

process_error_t_() ->
    [ ?_assertMatch({error, {jq_err_process, _}}, jq:parse(<<".[1]">>, <<"{}">>))
    , ?_assertMatch({error, {jq_err_process, _}}, jq:parse(<<".a">>, <<"[1,2]">>))
    ].
process_error_test_() -> wrap_setup_cleanup(process_error_t_()).

object_identifier_index_t_() ->
    [ ?_assertEqual({ok,[<<"{\"b\":2}">>]}, jq:parse(<<".">>, <<"{\"b\": 2}">>))
    , ?_assertEqual({ok,[<<"{\"b\":2}">>]}, jq:parse(<<".">>, <<"{\"b\":\n 2}">>))
    , ?_assertEqual({ok,[<<"2">>]}, jq:parse(<<".b">>, <<"{\"b\": 2}">>))
    , ?_assertEqual({ok,[<<"2">>]}, jq:parse(<<".a.b">>, <<"{\"a\":{\"b\": 2}}">>))
    ].
object_identifier_index_test_() -> wrap_setup_cleanup(object_identifier_index_t_()).

array_index_t_() ->
    [ ?_assertEqual({ok,[<<"1">>,<<"2">>,<<"3">>]}, jq:parse(<<".b|.[]">>, <<"{\"b\": [1,2,3]}">>))
    ].
array_index_test_() -> wrap_setup_cleanup(array_index_t_()).

test_prog(ExpectedResStr, FilterProgStr, InputStr) ->
    ExpectedResBin = erlang:list_to_binary(ExpectedResStr),
    FilterProgBin = erlang:list_to_binary(FilterProgStr),
    InputBin = erlang:list_to_binary(InputStr),
    ?_assertEqual({ok, [ExpectedResBin]},
                  jq:parse(
                    FilterProgBin,
                    InputBin)).

advanced_filter_programs_t() ->
    %% Programs here taken from https://jqplay.org/
    [
     test_prog(
       "\"many\"",
       "if . == 0 then \"zero\" elif . == 1 then \"one\" else \"many\" end",
       "2"),
     test_prog(
       "\"The input was 42, which is one less than 43\"",
       "\"The input was \\(.), which is one less than \\(.+1)\"",
       "42"),
     test_prog(
       "\"The input was 42, which is one less than 43\"",
       "\"The input was \\(.), which is one less than \\(.+1)\"",
       "42"),
     test_prog(
       "[2,3,4]",
       "map(.+1)",
       "[1,2,3]"),
     test_prog(
       "[5,3,7]",
       "map(select(. >= 2))",
       "[1,5,3,0,7]"),
     test_prog(
       "[\"JSON\",\"XML\"]",
       "[.[] | .name]",
       "[{\"name\":\"JSON\", \"good\":true}, {\"name\":\"XML\", \"good\":false}]"),
     test_prog(
       "[42,\"something else\"]",
       "[.foo, .bar]",
       "{ \"foo\": 42, \"bar\": \"something else\", \"baz\": true}"),
     test_prog(
       "[\"Foo\",\"abc\",\"abcd\"]",
       "keys",
       "{\"abc\": 1, \"abcd\": 2, \"Foo\": 3}"),
     test_prog(
       "[2,6,1,0]",
       "[.[] | length]",
       "[[1,2], \"string\", {\"a\":2}, null]"),
     test_prog(
       "[{\"user\":\"stedolan\",\"title\":\"JQ Primer\"},{\"user\":\"stedolan\",\"title\":\"More JQ\"}]",
       "[{user, title: .titles[]}]",
       "{\"user\":\"stedolan\",\"titles\":[\"JQ Primer\", \"More JQ\"]}"),
     test_prog(
       "42",
       ".foo",
       "{\"foo\": 42, \"bar\": \"less interesting data\"}"),
     test_prog(
       "{\"name\":\"XML\",\"good\":false}",
       ".[1]",
       "[{\"name\":\"JSON\", \"good\":true}, {\"name\":\"XML\", \"good\":false}]")
    ].
advanced_filter_programs_test_() ->
    wrap_setup_cleanup(advanced_filter_programs_t()).

get_tests_cases() ->
    [ erlang:element(2, Test) ||
      Test <-
      lists:flatten([empty_input_t_(), 
                     parse_error_t_(),
                     process_error_t_(),
                     object_identifier_index_t_(),
                     array_index_t_(),
                     advanced_filter_programs_t()
                    ])
    ].

repeat_tests(Parent, ShouldStop, [], AllTestFuns, Cnt) ->
    case counters:get(ShouldStop, 1) of
        0 ->
            repeat_tests(Parent, ShouldStop, AllTestFuns, AllTestFuns, Cnt);
        _ -> 
            Parent ! {test_process_stopped, Cnt}
    end;
repeat_tests(Parent, ShouldStop, [TestFun | RemTestFuns], AllTestFuns, Cnt) ->
    TestFun(),
    repeat_tests(Parent, ShouldStop, RemTestFuns, AllTestFuns, Cnt + 1).

concurrent_queries_test(NrOfTestProcesses, PrintThroughput, CacheSize, TestTimeMs) ->
    ShouldStop = counters:new(1, []),
    TestCases = get_tests_cases(), 
    Self = erlang:self(),
    % OldCacheSize = jq:get_filter_program_lru_cache_max_size(),
    % ok = jq:set_filter_program_lru_cache_max_size(CacheSize),
    TestRunner = fun() ->
                    repeat_tests(Self, ShouldStop, TestCases, TestCases, 0)
                 end,
    [erlang:spawn_link(TestRunner) || _ <- lists:seq(1, NrOfTestProcesses)], 
    timer:sleep(TestTimeMs),
    counters:add(ShouldStop, 1, 1),
    Cnts = [receive {test_process_stopped, Cnt} -> Cnt end || _ <- lists:seq(1, NrOfTestProcesses)], 
    case PrintThroughput of
        true ->
            Throughput = erlang:floor(lists:sum(Cnts) / (TestTimeMs / 1000)),
            erlang:display({'# of processes',
                            NrOfTestProcesses,
                            'Test Cases / Second',
                            Throughput,
                            'cache size',
                            CacheSize});
        false -> ok
    end,
    % ok = jq:set_filter_program_lru_cache_max_size(OldCacheSize),
    ok.

qubes_helper(0, SoFar) ->
    SoFar;
qubes_helper(N, SoFar) ->
    qubes_helper(N div 2, [N | SoFar]).
qubes(N) ->
    qubes_helper(N, []).

concurrent_queries_t_() ->
    {timeout, erlang:system_info(schedulers) * 14,
     fun() ->
             NrOfScheds = erlang:system_info(schedulers),
             Qubes = qubes(NrOfScheds),
             erlang:display_nl(),
             [(ok = concurrent_queries_test(NrOfTestProcess, true, 500, 500))
              || NrOfTestProcess <- Qubes],
             ok = concurrent_queries_test(NrOfScheds, false, 0, 100),
             ok = concurrent_queries_test(NrOfScheds, false, 1, 100),
             ok = concurrent_queries_test(NrOfScheds, false, 3, 100),
             ok = concurrent_queries_test(NrOfScheds, false, 10, 100),
             ok = concurrent_queries_test(NrOfScheds, false, 2, 100),
             jq_port:start_recording("./test/my_test_record.bin"),
             ok = concurrent_queries_test(NrOfScheds, false, 100, 300),
             jq_port:stop(),
             ok
     end}.
concurrent_queries_test_() -> wrap_setup_cleanup(concurrent_queries_t_()).

setup() ->
    jq_port:start(""),
    ok.

cleanup(_) ->
    jq_port:stop(),
    true = code:delete(jq),
    true = code:soft_purge(jq).
