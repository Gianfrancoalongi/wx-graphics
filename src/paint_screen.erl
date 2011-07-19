%%%-------------------------------------------------------------------
%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2011, Gianfranco
%%% Created : 16 Jul 2011 by Gianfranco <zenon@zen.local>
%%%-------------------------------------------------------------------
-module(paint_screen).
-behaviour(gen_server).
-include_lib("wx/include/wx.hrl").

%% API
-export([start_link/0]).
-export([add_to_paint_screen/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {canvas :: #wx{}}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec(add_to_paint_screen(integer(),term(),{integer(),integer()},#wx{}) ->
	     ok).
add_to_paint_screen(Layer,Id,Position,WxBitmap) ->    
    ets:insert(sprites,{{Layer,Id},Position,WxBitmap}),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ets:new(sprites,[public,named_table,ordered_set]),
    wx:set_env(view_screen:get_wxenv()),
    timer:send_interval(10,self(),redraw),    
    {ok, #state{canvas = view_screen:get_canvas()}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(redraw, #state{canvas = Canvas} = State) ->
    ViewScreenPos = view_screen:get_pos(),
    {W,H} = ViewScreenDimensions = wxWindow:getSize(Canvas),
    BitMap = wxBitmap:new(W,H),
    {Width,Height} = {wxBitmap:getWidth(BitMap),wxBitmap:getWidth(BitMap)},
    CDC = wxWindowDC:new(Canvas),
    MemoryDC = wxMemoryDC:new(BitMap),
    wxDC:clear(MemoryDC),
    ets:foldl(
      fun({{_Key,_Id},FramePos,Frame},_) ->
	      FrameDimensions = {wxBitmap:getWidth(Frame),wxBitmap:getHeight(Frame)},
	      case is_visible(ViewScreenPos,
			      ViewScreenDimensions,
			      FramePos,
			      FrameDimensions) of
		  true ->		      
		      NewPos = pos_to_view_screen_pos(ViewScreenPos,
						      FramePos),
		      wxDC:drawBitmap(MemoryDC, Frame, NewPos);
		  false ->
		      ignore
	      end
      end,unused,sprites),
    wxDC:blit(CDC,{0,0},{Width,Height},MemoryDC, {0,0},[{rop,?wxCOPY}]),
    wxWindowDC:destroy(CDC),
    wxMemoryDC:destroy(MemoryDC),
    wxBitmap:destroy(BitMap),    
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
is_visible({VS_X,VS_Y},{VS_W,VS_H},{F_X,F_Y},{F_W,F_H}) ->
    VSL = VS_X,
    VSR = VS_X + VS_W,
    VST = VS_Y,
    VSB = VS_Y + VS_H,
    FL = F_X,
    FR = F_X + F_W,
    FT = F_Y,
    FB = F_Y + F_H,
    not ((FL >= VSR) or (FR =< VSL) or (FT >= VSB) or (FB =< VST)).

pos_to_view_screen_pos({VS_X,VS_Y},{F_X,F_Y}) ->
    {F_X - VS_X,F_Y - VS_Y}.

%% -------------------------------- Wx New (Server)
%% ----------------------- Frame
%% ----------------------- Panel
%%  ---------------------  Canvas
%%  ---------------------  wxMindowDC ( Canvas )


%%  ---------------------  Bitmap (off scren buffer)
%%  ---------------------  wxMemoryDC ( Bitmap )  <---- draw on this, copy to canvas DC
