%%%-------------------------------------------------------------------
%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2011, Gianfranco
%%% Created : 12 Jul 2011 by Gianfranco <zenon@zen.local>
%%%-------------------------------------------------------------------
-module(human_entity).
-behaviour(gen_server).
-include("animation.hrl").

%% API
-export([add_to_graphics/1]).
-export([start_link/2]).
-export([position/1]).
-export([wander/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(entity,
	{x_pos :: integer(),
	 y_pos:: integer(),
	 pid :: pid(),
	 animations :: [{atom(),#animation{}}],
	 id :: integer()
	}).

%%%===================================================================
%%% API
%%%===================================================================
add_to_graphics(Id) ->
    Priv = code:priv_dir(graphics),
    {SexDir,SexPrefix} = lists:nth(random:uniform(2),[{"Guys","guy_"},
						      {"Girls","girl_"}]),
    Number = integer_to_list(random:uniform(14)),
    Path = filename:join([Priv,SexDir,SexPrefix++Number++".png"]),
    entity_sup:start_entity(?MODULE,[{Path,Id}]).

start_link(WxEnv,{SpritePath,Id}) ->
    gen_server:start_link({local,Id}, ?MODULE, {WxEnv,SpritePath,Id}, []).

position(Human) ->
    gen_server:call(Human,position).

wander(Human,{XS,XE}) ->
    gen_server:cast(Human,{wander,XS,XE}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({WxEnv,SpritePath,Id}) ->
    random:seed(now()),
    wx:set_env(WxEnv),
    [Down,Left,Right,Up] = sprite_lib:get_animations(SpritePath,
						     [{square,32},
						      {rows,4},
						      {columns,3}]),
    Animations = [ X#animation{frame_pauses = 100}
		   || X <-[Up#animation{offset_y_per_frame = -5},
			   Left#animation{offset_x_per_frame = -5},
			   Right#animation{offset_x_per_frame = 5},
			   Down#animation{offset_y_per_frame = 5}]],
    Bindings = [up,left,right,down],
    {ok, #entity{animations = lists:zip(Bindings,Animations),
		 id = Id,
		 x_pos = random:uniform(500),
		 y_pos = random:uniform(100)}}.

handle_call(position, _, State) ->
    {reply, {State#entity.x_pos,
	     State#entity.y_pos},
     State}.

handle_cast({wander,XS,XE}, #entity{x_pos = X} = State) ->
    case {X =< XS, X >= XE} of
	{true,_ } -> self() ! {animate,right};
	{ _ , true } -> self() ! {animate,left};
	{false,false} ->	    
	    case random:uniform(3) of
		1 -> timer:sleep(1000);
		2 ->  self() ! {animate,left};
		3 -> self() ! {animate,right}
	    end
    end,
    gen_server:cast(self(),{wander,XS,XE}),
    {noreply, State}.

handle_info({animate,Binding}, State) ->
    NewState = 
	case proplists:get_value(Binding,State#entity.animations) of
	    undefined -> State;
	    Animation ->
		animate(State,Animation)
	end,
    {noreply,NewState};
handle_info(_,State) -> {noreply,State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
animate(State,#animation{frames = []}) -> State;
animate(#entity{x_pos = X,y_pos = Y,id = Id} = State,
	#animation{frames = [Frame|Frames],
		   frame_pauses = FramePause,
		   offset_x_per_frame = Xd,
		   offset_y_per_frame = Yd
		  } = Animation) ->
    NewPos = {X + Xd,Y + Yd},
    paint_screen:add_to_paint_screen(2,Id,NewPos,Frame),
    timer:sleep(FramePause),
    animate(State#entity{x_pos = X + Xd,
			 y_pos = Y + Yd},
	    Animation#animation{frames=Frames}).
