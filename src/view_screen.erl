%%%-------------------------------------------------------------------
%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2011, Gianfranco
%%% Created : 15 Jul 2011 by Gianfranco <zenon@zen.local>
%%%-------------------------------------------------------------------
-module(view_screen).
-behaviour(gen_server).
-include_lib("wx/include/wx.hrl").

%% API
-export([start_link/0]).
-export([get_canvas/0]).
-export([get_pos/0]).
-export([get_wxenv/0]).
-export([move/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {x_pos :: integer(),
		y_pos :: integer(),
		canvas :: #wx{}
	       }).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local,?MODULE},?MODULE, [], []).

get_canvas() ->
    gen_server:call(?MODULE,get_canvas).

get_pos() ->
    gen_server:call(?MODULE,get_pos).

get_wxenv() ->
    gen_server:call(?MODULE,get_wxenv).

-spec(move(up|down|left|right) -> ok).	     
move(Dir) ->
    gen_server:call(?MODULE,{move,Dir}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok,Size} = application:get_env(graphics,view_screen_size),
    {ok,{X,Y}} = application:get_env(graphics,view_screen_pos),
    {ok,TitleStr} = application:get_env(graphics,view_screen_title),
    {ok, #state{canvas = make_window(TitleStr,Size),
		x_pos = X,
		y_pos = Y
	       }}.

handle_call(get_canvas, _, State) ->    
    {reply, State#state.canvas, State};
handle_call(get_pos,_,State) -> 
    {reply, {State#state.x_pos,State#state.y_pos},State};
handle_call(get_wxenv,_,State) -> 
    {reply, wx:get_env(), State};
handle_call({move,up},_, #state{y_pos = Y} = State) -> 
    {reply, ok, State#state{y_pos = Y - 5}};
handle_call({move,down},_, #state{y_pos = Y} = State) -> 
    {reply, ok, State#state{y_pos = Y + 5}};
handle_call({move,left},_, #state{x_pos = X} = State) -> 
    {reply, ok, State#state{x_pos = X - 5}};
handle_call({move,right},_,#state{x_pos = X} = State) -> 
    {reply, ok, State#state{x_pos = X + 5}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_window(TitleStr, Dimensions) ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, TitleStr, [{size,Dimensions}]),
    Canvas  = wxPanel:new(Frame),
    wxFrame:show(Frame),
    Canvas.
