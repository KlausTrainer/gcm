%% @doc GCM registration storage.
-module(gcm_db).

-behaviour(gen_server).

%% API
-export([start/1, start_link/1, stop/0]).
-export([list_keys/0, lookup/1, delete/1, store/2]).

%% gen_server callbacks
-export([
    init/1, handle_call/3, handle_info/2, handle_cast/2,
    code_change/3, terminate/2
]).


%% external API

-spec start(string()) -> {ok, pid()} | ignore | {error, term()}.
start(DbPath) ->
    gen_server:start({local, ?MODULE}, ?MODULE,
                          [DbPath], []).

-spec start_link(string()) -> {ok, pid()} | ignore | {error, term()}.
start_link(DbPath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [DbPath], []).

-spec stop() -> ok.
stop() ->
    catch gen_server:call(?MODULE, stop),
    ok.

-spec list_keys() -> [binary()].
list_keys() ->
    gen_server:call(?MODULE, list_keys, infinity).

-spec lookup(binary()) -> not_found | {ok, binary()}.
lookup(RegId) when is_binary(RegId) ->
    case gen_server:call(?MODULE, {lookup, RegId}, infinity) of
    not_found -> not_found;
    {ok, Result} -> {ok, Result}
    end.

-spec delete(binary()) -> ok.
delete(RegId) when is_binary(RegId) ->
    gen_server:call(?MODULE, {delete, RegId}, infinity).

-spec store(binary(), binary()) -> ok.
store(RegId, UserData) when is_binary(RegId), is_binary(UserData) ->
    gen_server:call(?MODULE, {store, RegId, UserData}, infinity).


%% gen_server callbacks

init([DbPath]) ->
    {ok, bitcask:open(DbPath, [read_write, sync_on_put])}.


handle_call(list_keys, _From, State) ->
    {reply, bitcask:list_keys(State), State};

handle_call({lookup, RegId}, _From, State) ->
    {reply, bitcask:get(State, RegId), State};

handle_call({delete, RegId}, _From, State) ->
    {reply, bitcask:delete(State, RegId), State};

handle_call({store, RegId, Value}, _From, State) ->
    {reply, bitcask:put(State, RegId, Value), State};

handle_call(stop, _From, State) ->
    bitcask:close(State),
    {stop, normal, ok, []}.


handle_cast(_Req, State) ->
    {noreply, State}.


handle_info(timeout, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, State) ->
    bitcask:close(State).
