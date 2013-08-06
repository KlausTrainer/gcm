-module(gcm_sender).

-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0, push/2, push_broadcast/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	     terminate/2, code_change/3]).

-define(BASE_URL, "http://android.googleapis.com/gcm/send").

-record(state, {
    key :: binary(),
    retry_after :: non_neg_integer()
}).


%% external API

start() ->
    case gcm:get_app_env(api_key) of
    undefined -> exit(no_api_key);
    ApiKey -> gen_server:start({local, ?MODULE}, ?MODULE, [ApiKey], [])
    end.

start_link() ->
    case gcm:get_app_env(api_key) of
    undefined -> exit(no_api_key);
    ApiKey -> gen_server:start_link({local, ?MODULE}, ?MODULE, [ApiKey], [])
    end.

stop() ->
    gen_server:call(?MODULE, stop).

-spec push([binary()], binary()) -> ok.
push(RegIds, Message) ->
    MyRegIds = lists:filter(
                   fun(RegId) ->
                       case gcm_db:lookup(RegId) of
                       not_found -> false;
                       _ -> true
                       end
                   end, RegIds),
    gen_server:cast(?MODULE, {send, MyRegIds, Message}).

-spec push_broadcast(binary()) -> ok.
push_broadcast(Message) ->
    gen_server:cast(?MODULE, {send, gcm_db:list_keys(), Message}).


%% gen_server callbacks

init([ApiKey]) ->
    {ok, #state{key=ApiKey, retry_after=0}}.


handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.


handle_cast({send, RegIds, Message}, #state{key=ApiKey} = State) ->
    Request = build_gcm_request(Message, RegIds),
    RequestHeaders = [{"Authorization", "key=" ++ ApiKey}, {"Content-Type", "application/json"}],
    Options = [{response_format, binary}],
    case ibrowse:send_req(?BASE_URL, RequestHeaders, post, Request, Options) of
    {ok, "200", _ResponseHeaders, Response} ->
        {Props} = jiffy:decode(Response),
        {_Multicast, _Success, Failure, Canonical, Results} = get_response_fields(Props),
        case to_be_parsed(Failure, Canonical) of
        true ->
            parse_results(Results, RegIds),
            {noreply, State};
        false ->
            {noreply, State}
        end;
    {error, Reason} ->
        lager:error("error: ~p", [Reason]),
        {noreply, State};
    {ok, "401", _, _} ->
        {stop, authorization, State};
    {ok, StatusCode, _, _} ->
        lager:error("error response status code ~p", [StatusCode]),
        {noreply, State}
    end.


handle_info(timeout, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% internal API

build_gcm_request(Message, RegIds) ->
    jiffy:encode({[{<<"registration_ids">>, RegIds},
                   {<<"data">>, {[{<<"message">>, Message}]}}]}).

get_response_fields(Json) ->
    Multicast = proplists:get_value(<<"multicast_id">>, Json),
    Success = proplists:get_value(<<"success">>, Json),
    Failure = proplists:get_value(<<"failure">>, Json),
    Canonical = proplists:get_value(<<"canonical_ids">>, Json),
    Results = proplists:get_value(<<"results">>, Json),
    {Multicast, Success, Failure, Canonical, Results}.

to_be_parsed(Failure, Canonical) ->
    case {Failure, Canonical} of
    {0, 0} -> false;
    _ -> true
    end.

parse_results([Result|Results], [RegId|RegIds]) ->
    case Result of
    {[{<<"error">>, Error}]} ->
        handle_error(Error, RegId),
        parse_results(Results, RegIds);
    {[{<<"message_id">>, _Id}]} ->
        parse_results(Results, RegIds);
    {[{<<"message_id">>, _Id}, {<<"registration_id">>, NewRegId}]} ->
        gcm_registration_handler:update_registration(RegId, NewRegId),
        lager:info("updated registration id ~p with new id ~p", [RegId, NewRegId]),
        parse_results(Results, RegIds);
    {[{<<"registration_id">>, NewRegId}, {<<"message_id">>, _Id}]} ->
        gcm_registration_handler:update_registration(RegId, NewRegId),
        lager:info("updated registration id ~p with new id ~p", [RegId, NewRegId]),
        parse_results(Results, RegIds)
    end;
parse_results([], []) ->
    ok.

handle_error(<<"Unavailable">>, _RegId) ->
    lager:error("unavailable"),
    ok;
handle_error(<<"InternalServerError">>, _RegId) ->
    lager:error("internal server error"),
    ok;
handle_error(<<"InvalidRegistration">>, RegId) ->
    gcm_registration_handler:unregister(RegId),
    lager:info("removed invalid registration: ~p", [RegId]);
handle_error(<<"NotRegistered">>, RegId) ->
    gcm_registration_handler:unregister(RegId),
    lager:info("removed registration: ~p", [RegId]);
handle_error(UnexpectedError, _RegId) ->
    lager:alert("unexpected error: ~p", [UnexpectedError]),
    ok.
