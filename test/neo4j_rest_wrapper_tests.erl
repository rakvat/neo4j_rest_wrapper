-module(neo4j_rest_wrapper_tests).
-include_lib("eunit/include/eunit.hrl").

basic_logic_test() ->
    ?assertEqual(4, 2+2).

with_inets_test_() ->
    {setup, fun setup/0, fun tear_down/1,
     [fun neo4j_version/0,
      fun cypher/0,
      fun get_node/0,
      fun create_node/0,
      fun delete_node/0
      ]}.

neo4j_version() ->
    ?assertEqual("2.0.3", neo4j_rest_wrapper:get_version()).

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
    Result = neo4j_rest_wrapper:get_node(0),
    Data = proplists:get_value(<<"data">>, Result),
    {struct, _Properties} = Data,
    % [{<<"name">>,<<"rakvat">>}]
    ok.

create_node() ->
    Properties = {struct, [{name, <<"rakvat">>}, {age, 77}]},
    Result = neo4j_rest_wrapper:create_node(Properties),
    Data = proplists:get_value(<<"data">>, Result),
    {struct, NodeData} = Data,
    SortedNodeData = lists:sort(NodeData),
    Expected = [{<<"name">>, <<"rakvat">>}, {<<"age">>, 77}],
    SortedExpected = lists:sort(Expected),
    SortedExpected = SortedNodeData,
    ok.

delete_node() ->
    % TODO: create node that will be deleted
    % or use mocks
    Result = neo4j_rest_wrapper:delete_node(0),
    % TODO: test that node is not available anymore
    ok. 


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
    neo4j_rest_wrapper:start("http://0.0.0.0:7474").
tear_down(_) ->
    neo4j_rest_wrapper:stop().

