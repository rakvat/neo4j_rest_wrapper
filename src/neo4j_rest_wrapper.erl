-module(neo4j_rest_wrapper).
-vsn("0.0.1").
-behavior(gen_server).


%% API
-export([start/0, start/1, stop/0, 
         get_version/0, 
         cypher/1,
         get_node/1,
         delete_node/1,
         create_node/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(config, {neo4j_url="http://0.0.0.0:7474"}).
-record(state, {config=#config{}}).
-define(SERVER, ?MODULE).

start() ->
    inets:start(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start(Neo4jUrl) ->
    inets:start(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Neo4jUrl], []).

stop() ->
    inets:stop().

get_version() ->
    gen_server:call(?MODULE, version).

cypher(Query) ->
    gen_server:call(?MODULE, {cypher, Query}).

get_node(NodeId) ->
    gen_server:call(?MODULE, {get_node, NodeId}).

delete_node(NodeId) ->
    gen_server:call(?MODULE, {delete_node, NodeId}).

% Properties should be in format {struct,[{key2,"?"}]}
create_node(Properties) ->
    gen_server:call(?MODULE, {create_node, Properties}).

%%%%%%%%%%%%%%%%%%%%%%%%%% gen_server callbacks
init(Args) ->
    case Args of
        [] -> {ok, #state{}};
        [Neo4jUrl] -> {ok, #state{config=#config{neo4j_url=Neo4jUrl}}}
    end.

handle_call(version, _From, State) ->
    {ok, Response} = httpc:request(State#state.config#config.neo4j_url ++ "/db/data/"),
    {_Status, _Headers, Body} = Response,
    Parsed = mochijson2:decode(Body),
    {struct, Data} = Parsed,
    Version = proplists:get_value(<<"neo4j_version">>, Data),
    Result = binary_to_list(Version),
    {reply, Result, State};

handle_call({cypher, Query}, _From, State) ->
% TODO: params in query
    Url = State#state.config#config.neo4j_url ++ "/db/data/cypher",
    Headers = [],
    Type = "application/json",
    Body = "{\"query\": \"" ++ Query ++ "\"}",
    HTTPOptions = [],
    Options = [],
    Request = {Url, Headers, Type, Body},
    Result = httpc:request(post, Request, HTTPOptions, Options),
    {ok, Response} = Result,
    {_Status, _Headers, ResultBody} = Response,
    Parsed = mochijson2:decode(ResultBody),
    {struct, Data} = Parsed,
    Data,
    {reply, Data, State};

handle_call({get_node, NodeId}, _From, State) ->
    Url = State#state.config#config.neo4j_url ++ "/db/data/node/" ++ integer_to_list(NodeId),
    Headers = [],
    HTTPOptions = [],
    Options = [],
    Request = {Url, Headers},
    Result = httpc:request(get, Request, HTTPOptions, Options),
    {ok, Response} = Result,
    {_Status, _Headers, ResultBody} = Response,
    Parsed = mochijson2:decode(ResultBody),
    {struct, Data} = Parsed,
    Data,
    {reply, Data, State};

handle_call({delete_node, NodeId}, _From, State) ->
    Url = State#state.config#config.neo4j_url ++ "/db/data/node/" ++ integer_to_list(NodeId),
    Headers = [],
    HTTPOptions = [],
    Options = [],
    Request = {Url, Headers},
    Result = httpc:request(delete, Request, HTTPOptions, Options),
    {ok, _Response} = Result,
    {reply, {}, State};

handle_call({create_node, Properties}, _From, State) ->
    Url = State#state.config#config.neo4j_url ++ "/db/data/node",
    Headers = [],
    Type = "application/json",
    Body = iolist_to_binary(mochijson2:encode(Properties)),
    HTTPOptions = [],
    Options = [],
    Request = {Url, Headers, Type, Body},
    Result = httpc:request(post, Request, HTTPOptions, Options),
    {ok, Response} = Result,
    {_Status, _Headers, ResultBody} = Response,
    Parsed = mochijson2:decode(ResultBody),
    {struct, Data} = Parsed,
    Data,
    {reply, Data, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
