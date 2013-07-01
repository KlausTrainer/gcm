-module(gcm_web).

%% API
-export([start/1, stop/1]).

-spec start(inet:port_number()) -> {ok, pid()}.
start(Port) ->
    PrivDir = code:priv_dir(gcm),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/registrations/[:registration_id]", gcm_registration_handler, []}
        ]}
    ]),
    cowboy:start_https(?MODULE, 64, [
        {port, Port},
        {cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
        {certfile, PrivDir ++ "/ssl/server.crt"},
        {keyfile, PrivDir ++ "/ssl/server.key"}
    ], [{env, [{dispatch, Dispatch}]}]).

stop(Pid) ->
    cowboy:stop_listener(Pid).
