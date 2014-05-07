-module(neo4j_rest_wrapper_tests).
-include_lib("eunit/include/eunit.hrl").

basic_logic_test() ->
    ?assertEqual(4, 2+2).

with_inets_test_() ->
    {setup, fun setup/0, fun tear_down/1,
     [fun neo4j_version/0,
      fun cypher/0]}.

neo4j_version() ->
    ?assertEqual("2.0.3", neo4j_rest_wrapper:get_version()).

cypher() ->
    Query="START n=node(*) RETURN n;",
    Result = neo4j_rest_wrapper:cypher(Query),
    Columns = proplists:get_value(<<"columns">>, Result),
    ?assertEqual([<<"n">>], Columns),
    Data = proplists:get_value(<<"data">>, Result),
    [FirstRow|_] = Data,
    [{struct, FirstRowResult}] = FirstRow,
    FirstRowData = proplists:get_value(<<"data">>, FirstRowResult),
    {struct, FirstRowDataContent} = FirstRowData,
    ok.

setup() ->
    inets:start().
tear_down(_) ->
    inets:stop().

