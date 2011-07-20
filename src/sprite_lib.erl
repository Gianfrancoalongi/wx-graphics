-module(sprite_lib).
-include("animation.hrl").
-export([get_animations/2]).
-export([get_frames/2]).
-export([x_move_per_frame/2,
	 y_move_per_frame/2]).
-export([frame_paint_offset_aligned/3]).

-spec(get_animations(string(),[{atom(),term()}]) -> [#animation{}]). 
get_animations(FileName,Options) ->
    Side = proplists:get_value(square,Options),
    Rows = proplists:get_value(rows,Options),
    Columns = proplists:get_value(columns,Options),
    get_animations(FileName,Side,Rows,Columns).

get_animations(SpriteFile,SquareSize,Rows,Columns) ->
    Img = wxImage:new(SpriteFile),
    BitMap = wxBitmap:new(Img),    
    Anims = [ begin
		  Frames = [ #frame{bitmap = wxBitmap:getSubBitmap(BitMap,
								   {Column*SquareSize,
								    Row*SquareSize,
								    SquareSize,
								    SquareSize}),
				    x_paint_offset = 0,
				    y_paint_offset = 0,
				    x_move = 0,
				    y_move = 0,
				    frame_pause = 80
				   }
				    || Column <- lists:seq(0,Columns-1)],
		  #animation{frames = Frames}
	      end || Row <- lists:seq(0,Rows-1)],
    wxBitmap:destroy(BitMap),
    wxImage:destroy(Img),
    Anims.

-spec(get_frames(string(),[{integer(),integer(),integer(),integer()}]) -> [#frame{}]).
get_frames(FileName,Areas) ->
    Img = wxImage:new(FileName),
    BitMap = wxBitmap:new(Img),
    SubBitMaps = [ wxBitmap:getSubBitmap(BitMap,{X,Y,W,H}) || {X,Y,W,H} <- Areas ],    
    wxBitmap:destroy(BitMap),    
    wxImage:destroy(Img),
    [#frame{bitmap = SubBitMap,
	    x_paint_offset = 0,
	    y_paint_offset = 0,
	    x_move = 0,
	    y_move = 0,
	    frame_pause = 80
	   } || SubBitMap <- SubBitMaps].

-spec(x_move_per_frame(#animation{},integer()) -> #animation{}).
x_move_per_frame(#animation{frames = Frames} = Animation,OffsetX) ->
    NewFrames = [ Frame#frame{x_move = OffsetX} || Frame <- Frames ],
    Animation#animation{frames = NewFrames}.

-spec(y_move_per_frame(#animation{},integer()) -> #animation{}).
y_move_per_frame(#animation{frames = Frames} = Animation,OffsetY) ->	     
    NewFrames = [ Frame#frame{y_move = OffsetY} || Frame <- Frames ],
    Animation#animation{frames = NewFrames}.

-spec(frame_paint_offset_aligned({integer(),integer()},[term()],[#frame{}]) -> [#frame{}]).
frame_paint_offset_aligned(_,[],Frames) -> Frames;
frame_paint_offset_aligned({_,YRef}=Point,[{bottom,Bottom}|R],Frames)  ->
    NewFrames = lists:map(
		  fun(#frame{bitmap = BitMap} = Frame) ->			  
			  Height = wxBitmap:getHeight(BitMap),
			  Diff = Bottom - (YRef + Height),
			  Frame#frame{y_paint_offset = Diff}
		  end,Frames),
    frame_paint_offset_aligned(Point,R,NewFrames);
frame_paint_offset_aligned({XRef,_}=Point,[{centered_horizontally,MidX}|R],Frames) ->
    NewFrames = lists:map(
		  fun(#frame{bitmap = BitMap} = Frame) ->
			  Width = wxBitmap:getWidth(BitMap),
			  Diff = round(MidX - (XRef + (Width/2))),
			  Frame#frame{x_paint_offset = Diff}
		  end,Frames),
    frame_paint_offset_aligned(Point,R,NewFrames).
			  

