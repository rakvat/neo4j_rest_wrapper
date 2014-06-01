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
    Data = proplists:get_value(<<"data">>, Result),
    {struct, Properties} = Data,
    [{<<"a">>,<<"b">>}] = Properties,
    ?assert(meck:validate(httpc)),
    ?assertEqual(1, meck:num_calls(httpc, request, '_')).

get_missing_node() ->
    {error, Result} = neo4j_rest_wrapper:get_node(125234656),
    ?assert(meck:validate(httpc)),
    ?assertEqual(1, meck:num_calls(httpc, request, '_')).
% "{\n  \"message\" : \"Cannot find node with id [23] in database.\",\n  \"exception\" : \"NodeNotFoundException\",\n  \"fullname\" : \"org.neo4j.server.rest.web.NodeNotFoundException\",\n  \"stacktrace\" : [ \"org.neo4j.server.rest.web.DatabaseActions.node(DatabaseActions.java:183)\", \"org.neo4j.server.rest.web.DatabaseActions.getNode(DatabaseActions.java:228)\", \"org.neo4j.server.rest.web.RestfulGraphDatabase.getNode(RestfulGraphDatabase.java:265)\", \"java.lang.reflect.Method.invoke(Unknown Source)\", \"org.neo4j.server.rest.transactional.TransactionalRequestDispatcher.dispatch(TransactionalRequestDispatcher.java:139)\", \"org.neo4j.server.rest.security.SecurityFilter.doFilter(SecurityFilter.java:112)\", \"java.lang.Thread.run(Unknown Source)\" ]\n}"

create_node() ->
    Properties = {struct, [{name, <<"rakvat">>}, {age, 77}]},
    {ok, Result} = neo4j_rest_wrapper:create_node(Properties),
    Data = proplists:get_value(<<"data">>, Result),
    {struct, NodeData} = Data,
    SortedNodeData = lists:sort(NodeData),
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

% TODO: handle errors from neo4j
%[{<<"message">>,<<"Cannot find node with id [1] in database.">>},
% {<<"exception">>,<<"NodeNotFoundException">>},
% {<<"fullname">>,
%  <<"org.neo4j.server.rest.web.NodeNotFoundException">>},
% {<<"stacktrace">>,
%  [<<"org.neo4j.server.rest.web.DatabaseAction"...>>,
%   <<"org.neo4j.server.rest.web.DatabaseAc"...>>,
%   <<"org.neo4j.server.rest.web.Restfu"...>>,
%   <<"java.lang.reflect.Method.inv"...>>,
%   <<"org.neo4j.server.rest.tr"...>>,<<"org.neo4j.server.res"...>>,
%   <<"java.lang.Thread"...>>]}]
setup() ->
    meck:new(httpc, [passthrough]),
    neo4j_rest_wrapper:start("http://0.0.0.0:7474").
tear_down(_) ->
    meck:unload(httpc),
    neo4j_rest_wrapper:stop().

