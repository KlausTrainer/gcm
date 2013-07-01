-module(gcm_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    gcm_sup:start_link().

stop(_State) ->
    ok.
