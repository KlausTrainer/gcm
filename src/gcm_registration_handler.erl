%% @doc GCM registration handler.
-module(gcm_registration_handler).

-compile([{parse_transform, lager_transform}]).

%% API
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

-export([create_resource/2]).
-export([delete_resource/2]).

-export([register/1, unregister/1, update_registration/2]).

-define(RESPONSE_HEADERS, [{<<"content-type">>, <<"application/json; charset=utf-8">>}]).

%% external API

-spec init({ssl, http}, cowboy_req:req(), []) -> {upgrade, protocol, cowboy_rest}.
init({ssl, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"PUT">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{'*', create_resource}], Req, State}.

-spec register(binary()) -> ok.
register(RegId) ->
    ok = gcm_db:store(RegId, <<>>).

-spec unregister(binary()) -> ok.
unregister(RegId) ->
    ok = gcm_db:store(RegId, term_to_binary(unregistered)).

-spec update_registration(binary(), binary()) -> ok.
update_registration(OldRegId, NewRegId) ->
    ok = gcm_db:store(NewRegId, <<>>),
    ok = gcm_db:store(OldRegId, term_to_binary({updated, NewRegId})).


%% internal API

-spec create_resource(cowboy_req:req(), undefined) -> {halt, cowboy_req:req(), undefined}.
create_resource(Req, undefined) ->
    {ok, _Response} = case cowboy_req:bindings(Req) of
    {[{registration_id, RegId}], _} ->
        ?MODULE:register(RegId),
        lager:info("registered: ~p", [RegId]),
        cowboy_req:reply(200, ?RESPONSE_HEADERS, <<"{}">>, Req);
    _ ->
        cowboy_req:reply(400, ?RESPONSE_HEADERS, <<"{\"error\":\"bad request\"}">>, Req)
    end,
    {halt, Req, undefined}.

-spec delete_resource(cowboy_req:req(), undefined) -> {halt, cowboy_req:req(), undefined}.
delete_resource(Req, undefined) ->
    {ok, _Response} = case cowboy_req:bindings(Req) of
    {[{registration_id, RegId}], _} ->
        case gcm_db:lookup(RegId) of
        {ok, _} ->
            ?MODULE:unregister(RegId),
            lager:info("unregistered: ~p", [RegId]),
            cowboy_req:reply(200, ?RESPONSE_HEADERS, <<"{}">>, Req);
        not_found ->
            cowboy_req:reply(404, ?RESPONSE_HEADERS, <<"{\"error\":\"not found\"}">>, Req)
        end;
    _ ->
        cowboy_req:reply(400, ?RESPONSE_HEADERS, <<"{\"error\":\"missing registration_id\"}">>, Req)
    end,
    {halt, Req, undefined}.
