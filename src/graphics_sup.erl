%%%-------------------------------------------------------------------
%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2011, Gianfranco
%%% Created : 16 Jul 2011 by Gianfranco <zenon@zen.local>
%%%-------------------------------------------------------------------
-module(graphics_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    RestartStrategy = rest_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    ViewScreen = {view_screen, {view_screen, start_link, []},
		  Restart, Shutdown, Type, [view_screen]},
    PaintScreen = {paint_screen, {paint_screen, start_link, []},
		  Restart, Shutdown, Type, [paint_screen]},
    EntitySup = {entity_sup, {entity_sup, start_link, []},
		 Restart, Shutdown, supervisor, [paint_screen]},
    {ok, {SupFlags, [ViewScreen,PaintScreen,EntitySup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
