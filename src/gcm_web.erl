-module(gcm_web).


%% API
-export([start/0, stop/1]).

%% external API

-spec start() -> {ok, pid()}.
start() ->
    {ok, Application} = application:get_application(),
    PrivDir = code:priv_dir(Application),
    Port = Application:get_app_env(port, 9443),
    CaCertFile = Application:get_app_env(cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"),
    CertFile = Application:get_app_env(certfile, PrivDir ++ "/ssl/server.crt"),
    KeyFile = Application:get_app_env(keyfile, PrivDir ++ "/ssl/server.key"),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/registrations/[:registration_id]", gcm_registration_handler, []}
        ]}
    ]),
    cowboy:start_https(?MODULE, 64, [
        {port, Port},
        {cacertfile, CaCertFile},
        {certfile, CertFile},
        {keyfile, KeyFile},
        {ciphers, unbroken_cipher_suites()}
    ], [{env, [{dispatch, Dispatch}]}]).

stop(Pid) ->
    cowboy:stop_listener(Pid).


%% internal API

%% Unfortunately the implementation of elliptic-curve ciphers that has
%% been introduced in R16B01 is incomplete. Depending on the particular
%% client, this can cause the TLS handshake to break during key
%% agreement. Depending on the ssl application version, this function
%% returns a list of all cipher suites that are supported by default,
%% minus the elliptic-curve ones.
-spec unbroken_cipher_suites() -> [ssl:erl_cipher_suite()].
unbroken_cipher_suites() ->
    case proplists:get_value(ssl_app, ssl:versions()) of
    "5.3" ->
        lists:filter(fun(Suite) ->
        string:left(atom_to_list(element(1, Suite)), 4) =/= "ecdh"
    end, ssl:cipher_suites());
    _ ->
        ssl:cipher_suites()
    end.
