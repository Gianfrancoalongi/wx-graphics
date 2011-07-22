%%%-------------------------------------------------------------------
%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2011, Gianfranco
%%% Created : 17 Jul 2011 by Gianfranco <zenon@zen.local>
%%%-------------------------------------------------------------------
-module(entity_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_entity/2]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec(start_entity(atom(),[term()]) -> {'error',_} | 
				       {'ok','undefined' | pid()} | 
				       {'ok','undefined' | pid(),_}).
start_entity(Module,Arguments) ->
    supervisor:start_child(?MODULE,[Module|Arguments]).
    
%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    AChild = {entity,{entity, start_link, [view_screen:get_wxenv()]},
	      Restart, Shutdown, Type, [entity]},
    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
