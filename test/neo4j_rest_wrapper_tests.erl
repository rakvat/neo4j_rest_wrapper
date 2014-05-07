-module(neo4j_rest_wrapper_tests).
-include_lib("eunit/include/eunit.hrl").

basic_logic_test() ->
    ?assertEqual(4, 2+2).

dummy_test() ->
    ?assertEqual("xxx", neo4j_rest_wrapper:dummy()).

neo4j_version_test() ->
    {setup, fun setup/0, fun tear_down/1,
     ?_test( ?assertEqual("2.0.3", neo4j_rest_wrapper:get_version()))}.


setup() ->
    inets:start().
tear_down(_) ->
    inets:stop().

