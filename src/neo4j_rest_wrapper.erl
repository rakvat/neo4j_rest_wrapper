-module(neo4j_rest_wrapper).
-vsn("0.0.1").

-export([start/0, stop/0, dummy/0, get_version/0]).

% are start/stop needed here or is it ok to start inets 
% in application resource file?
start() ->
    inets:start().

stop() ->
    inets:stop().

dummy() ->
    "xxx".

get_version() ->
    {ok, Response} = httpc:request("http://0.0.0.0:7474/db/data/"),
    {_Status, _Headers, Body} = Response,
    Parsed = mochijson2:decode(Body),
    {struct, Data} = Parsed,
    Version = proplists:get_value(<<"neo4j_version">>, Data),
    binary_to_list(Version).

