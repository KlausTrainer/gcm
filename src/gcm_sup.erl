-module(gcm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Processes = [
        {
            gcm_db,
            {gcm_db, start_link, []},
            permanent, 2000, worker, dynamic
        },
        {
            gcm_web,
            {gcm_web, start, []},
            permanent, 2000, worker, dynamic
        },
        {
            gcm_sender,
            {gcm_sender, start_link, []},
            permanent, 2000, worker, dynamic
        }
    ],
    {ok, {{one_for_one, 5, 10}, Processes}}.
