%% @doc GCM registration storage.
-module(gcm_db).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0]).
-export([fold/2, lookup/1, delete/1, store/2]).

%% gen_server callbacks
-export([
    init/1, handle_call/3, handle_info/2, handle_cast/2,
    code_change/3, terminate/2
]).


%% external API

-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE,
                          [db_path()], []).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [db_path()], []).

-spec stop() -> ok.
stop() ->
    catch gen_server:call(?MODULE, stop),
    ok.

-spec fold(fun((binary(), binary(), term()) -> term()),
           term()) -> term() | {error, term()}.
fold(Fun, Acc0) ->
    gen_server:call(?MODULE, {fold, Fun, Acc0}, infinity).

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


handle_call({fold, Fun, Acc0}, _From, State) ->
    {reply, bitcask:fold(State, Fun, Acc0), State};

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


%% internal API

db_path() ->
    PrivDir = code:priv_dir(gcm),
    gcm:get_app_env(db_path, PrivDir ++ "/gcm_db.bitcask").
