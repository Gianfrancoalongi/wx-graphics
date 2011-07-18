%%%-------------------------------------------------------------------
%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2011, Gianfranco
%%% Created : 12 Jul 2011 by Gianfranco <zenon@zen.local>
%%%-------------------------------------------------------------------
-module(backround_entity).
-behaviour(gen_server).
-include("animation.hrl").
-include_lib("wx/include/wx.hrl").

%% API
-export([start_link/2]).

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
start_link(WxEnv,Id) ->
    gen_server:start_link({local,Id}, ?MODULE, 
			  {WxEnv,"./priv/Misc/megaman.gif",Id}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({WxEnv,SpritePath,Id}) ->
    wx:set_env(WxEnv),
    [Bg] = sprite_lib:get_frames(SpritePath,[{0,0,981,690}]),
    Animation = #animation{frames = [Bg],
			   frame_pauses = 1000,
			   offset_x_per_frame = 0,
			   offset_y_per_frame = 0
			  },
    State = #entity{animations = [Animation],
		    id = Id,
		    x_pos = 0,
		    y_pos = 0},
    animate(State,Animation),
    {ok,State}.

handle_call(_,_,State) ->
    {reply,ok,State}.

handle_cast(_,State) ->
    {noreply,State}.

handle_info(_,State) ->
    {noreply,State}.

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
    ets:insert(sprites,{100,Id,NewPos,Frame}),
    timer:sleep(FramePause),
    animate(State#entity{x_pos = X + Xd,
			 y_pos = Y + Yd},
	    Animation#animation{frames=Frames}).
