-module(sprite_lib).
-include("animation.hrl").
-export([get_animations/2]).
-export([get_frames/2]).

-spec(get_animations(string(),[{atom(),term()}]) -> [#animation{}]).	     
get_animations(FileName,Options) ->
    Side = proplists:get_value(square,Options),
    Rows = proplists:get_value(rows,Options),
    Columns = proplists:get_value(columns,Options),
    get_animations(FileName,Side,Rows,Columns).

get_animations(SpriteFile,SquareSize,Rows,Columns) ->
    Img = wxImage:new(SpriteFile),
    BitMap = wxBitmap:new(Img),    
    Anims = [ #animation{frames = [ wxBitmap:getSubBitmap(BitMap,
							  {Column*SquareSize,
							   Row*SquareSize,
							   SquareSize,
							   SquareSize})
				    || Column <- lists:seq(0,Columns-1)],
			 frame_pauses = 0,
			 offset_x_per_frame = 0,
			 offset_y_per_frame = 0}
	      || Row <- lists:seq(0,Rows-1)],
    wxBitmap:destroy(BitMap),
    wxImage:destroy(Img),
    Anims.

-spec(get_frames(string(),[{integer(),integer(),integer(),integer()}]) -> [#wx{}]).
get_frames(FileName,Areas) ->
    Img = wxImage:new(FileName),
    BitMap = wxBitmap:new(Img),
    Frames = [ wxBitmap:getSubBitmap(BitMap,{X,Y,W,H}) || {X,Y,W,H} <- Areas ],    
    wxBitmap:destroy(BitMap),    
    wxImage:destroy(Img),
    Frames.
