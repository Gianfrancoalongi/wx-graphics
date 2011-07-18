%%%-------------------------------------------------------------------
%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2011, Gianfranco
%%% Created : 12 Jul 2011 by Gianfranco <zenon@zen.local>
%%%-------------------------------------------------------------------
-module(animation_entity).
-behaviour(gen_server).
-include("animation.hrl").
-include_lib("wx/include/wx.hrl").

%% API
-export([start_link/3]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(entity,
	{x_pos :: integer(),
	 y_pos:: integer(),
	 pid :: pid(),
	 animations :: [{atom(),#animation{}}]
	}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(WxEnv,SpritePath,Name) ->
    gen_server:start_link({local, Name}, ?MODULE, {WxEnv,SpritePath}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({WxEnv,SpritePath}) ->
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
		 x_pos = 50,
		 y_pos = 20}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
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
animate(#entity{x_pos = X,y_pos = Y} = State,
	#animation{frames = [Frame|Frames],
		   frame_pauses = FramePause,
		   offset_x_per_frame = Xd,
		   offset_y_per_frame = Yd
		  } = Animation) ->
    NewPos = {X + Xd,Y + Yd},
    ets:insert(sprites,{self(),NewPos,Frame}),
    timer:sleep(FramePause),
    animate(State#entity{x_pos = X + Xd,
			 y_pos = Y + Yd},
	    Animation#animation{frames=Frames}).
