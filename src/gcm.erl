-module(gcm).

%% API
-export([start/0]).

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(bitcask),
    ok = application:start(ibrowse),
    ok = application:start(sasl),
    ok = application:start(lager),
    ok = application:start(gcm).
