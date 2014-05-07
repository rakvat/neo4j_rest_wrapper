-module(neo4j_rest_wrapper).
-vsn("0.0.1").

-export([start/0, stop/0, get_version/0, cypher/1]).

% are start/stop needed here or is it ok to start inets 
% in application resource file?
start() ->
    inets:start().

stop() ->
    inets:stop().

get_version() ->
    {ok, Response} = httpc:request("http://0.0.0.0:7474/db/data/"),
    {_Status, _Headers, Body} = Response,
    Parsed = mochijson2:decode(Body),
    {struct, Data} = Parsed,
    Version = proplists:get_value(<<"neo4j_version">>, Data),
    binary_to_list(Version).

% TODO: params in query
cypher(Query) ->
    Url = "http://0.0.0.0:7474/db/data/cypher",
    Headers = [],
    Type = "application/json",
    %TODO: use query
    Body = "{\"query\": \"" ++ Query ++ "\"}",
    HTTPOptions = [],
    Options = [],
    Request = {Url, Headers, Type, Body},
    Result = httpc:request(post, Request, HTTPOptions, Options),
    {ok, Response} = Result,
    {_Status, _Headers, ResultBody} = Response,
    Parsed = mochijson2:decode(ResultBody),
    {struct, Data} = Parsed,
    Data.

