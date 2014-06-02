-module(neo4j_rest_wrapper_tests).
-include_lib("eunit/include/eunit.hrl").

basic_logic_test() ->
    ?assertEqual(4, 2+2).

with_inets_test_() ->
    {foreach, fun setup/0, fun tear_down/1,
     [
      fun neo4j_version/0,
      fun cypher/0,
      fun get_node/0,
      fun get_missing_node/0,
      fun create_node/0,
      fun delete_node/0
      ]}.

neo4j_version() ->
    FakeJson = "{\"neo4j_version\" : \"2.0.23\"}",
    FakeResult = {ok, {x,x,FakeJson}},
    meck:expect(httpc, request, fun(_) -> FakeResult end),
    ?assertEqual("2.0.23", neo4j_rest_wrapper:get_version()),
    ?assert(meck:validate(httpc)),
    ?assertEqual(1, meck:num_calls(httpc, request, '_')).

cypher() ->
    Query="START n=node(*) RETURN n;",
    Result = neo4j_rest_wrapper:cypher(Query),
    % [{<<"columns">>,[<<"n">>]},
    %  {<<"data">>,
    %   [[{struct,[{<<"outgoing_relationships">>,
    %               <<"http://0.0.0.0:7474/db/d"...>>},
    %              {<<"labels">>,<<"http://0.0.0.0:7474/"...>>},
    %              {<<"data">>,{struct,[{<<...>>,...}]}},
    %              {<<"all_typed_relati"...>>,<<"http://0.0.0"...>>},
    %              {<<"traverse">>,<<"http://0"...>>},
    %              {<<"self">>,<<"http"...>>},
    %              {<<"prop"...>>,<<...>>},
    %              {<<...>>,...},
    %              {...}|...]}]]}]
    Columns = proplists:get_value(<<"columns">>, Result),
    ?assertEqual([<<"n">>], Columns),
    Data = proplists:get_value(<<"data">>, Result),
    [FirstRow|_] = Data,
    [{struct, FirstRowResult}] = FirstRow,
    FirstRowData = proplists:get_value(<<"data">>, FirstRowResult),
    {struct, _FirstRowDataContent} = FirstRowData,
    % [{<<"name">>,<<"rakvat">>}]
    ok.

get_node() ->
    FakeJson = "{\"data\" : {\"a\": \"b\"}}",
    FakeResult = {ok, {x,x,FakeJson}},
    meck:expect(httpc, request, fun(_) -> FakeResult end),
    {ok, Result} = neo4j_rest_wrapper:get_node(1),
    ?assertEqual([properties], dict:fetch_keys(Result)),
    Properties = dict:fetch(properties, Result),
    [{<<"a">>,<<"b">>}] = Properties,
    ?assert(meck:validate(httpc)),
    ?assertEqual(1, meck:num_calls(httpc, request, '_')).

get_missing_node() ->
    {error, Result} = neo4j_rest_wrapper:get_node(125234656),
    ?assertEqual(lists:sort([message, exception]), 
                 lists:sort(dict:fetch_keys(Result))),
    ?assertEqual(<<"NodeNotFoundException">>, 
                 dict:fetch(exception, Result)),
    ?assert(meck:validate(httpc)),
    ?assertEqual(1, meck:num_calls(httpc, request, '_')).

create_node() ->
    Properties = {struct, [{name, <<"rakvat">>}, {age, 77}]},
    {ok, Result} = neo4j_rest_wrapper:create_node(Properties),
    ?assertEqual([properties], dict:fetch_keys(Result)),
    ResultProperties = dict:fetch(properties, Result),
    SortedNodeData = lists:sort(ResultProperties),
    Expected = [{<<"name">>, <<"rakvat">>}, {<<"age">>, 77}],
    SortedExpected = lists:sort(Expected),
    ?assertEqual(SortedExpected, SortedNodeData),
    ok.

delete_node() ->
    % TODO: create node that will be deleted
    % or use mocks
    Result = neo4j_rest_wrapper:delete_node(0),
    % TODO: test that node is not available anymore
    ok. 

% TODO real neo4j connection roundtrip test

setup() ->
    meck:new(httpc, [passthrough]),
    neo4j_rest_wrapper:start("http://0.0.0.0:7474").
tear_down(_) ->
    meck:unload(httpc),
    neo4j_rest_wrapper:stop().

