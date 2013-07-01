%% @doc GCM registration handler.
-module(gcm_registration_handler).

-compile([{parse_transform, lager_transform}]).

%% API
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

-export([handle_request/2]).
-export([delete_resource/2]).

-export([register/1, unregister/1, update_registration/2]).

-define(RESPONSE_HEADERS, [{<<"content-type">>, <<"application/json; charset=utf-8">>}]).

%% external API

-spec init({ssl, http}, cowboy_req:req(), []) -> {upgrade, protocol, cowboy_rest}.
init({ssl, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, handle_request}], Req, State}.

-spec register(binary()) -> ok.
register(RegId) ->
    ok = gcm_db:store(RegId, <<>>).

-spec unregister(binary()) -> ok.
unregister(RegId) ->
    ok = gcm_db:delete(RegId).

-spec update_registration(binary(), binary()) -> ok.
update_registration(OldRegId, NewRegId) ->
    ok = gcm_db:delete(OldRegId),
    ok = gcm_db:store(NewRegId, <<>>).


%% internal API

-spec handle_request(cowboy_req:req(), undefined) -> {cowboy_req:req(), cowboy_req:req(), undefined}.
handle_request(Req, undefined) ->
    {ok, Body, _} = cowboy_req:body(Req),
    {ok, Res} = case parse_req_body(Body) of
    {registration_id, RegId} ->
        ?MODULE:register(RegId),
        lager:info("registered: ~p", [RegId]),
        cowboy_req:reply(200, ?RESPONSE_HEADERS, <<"{}">>, Req);
    error ->
        cowboy_req:reply(400, ?RESPONSE_HEADERS, <<"{\"error\":\"bad request\"}">>, Req)
    end,
    {Res, Req, undefined}.

-spec delete_resource(cowboy_req:req(), undefined) -> {cowboy_req:req(), cowboy_req:req(), undefined}.
delete_resource(Req, undefined) ->
    {ok, Res} = case cowboy_req:bindings(Req) of
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
    {Res, Req, undefined}.

parse_req_body(Body) ->
    try
        case jiffy:decode(Body) of
        {Props} ->
            case proplists:get_value(<<"registration_id">>, Props) of
            undefined -> error;
            RegId -> {registration_id, RegId}
            end;
        _ ->
            error
        end
    catch throw:_ ->
        error
    end.
