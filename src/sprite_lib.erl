-module(sprite_lib).
-include("animation.hrl").
-export([get_animations/2]).
-export([get_frames/2]).
-export([x_move_per_frame/2,
	 y_move_per_frame/2]).
-export([frame_paint_offset_aligned/3]).
-export([resize_keep_ratio/3]).
-export([rotate_frames/4]).

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
    SubBitMaps = [ begin
		       TmpBmp = wxBitmap:getSubBitmap(BitMap,{X,Y,W,H}),
		       TmpImg = wxBitmap:convertToImage(TmpBmp),
		       wxBitmap:destroy(TmpBmp),
		       BM = wxBitmap:new(TmpImg),
		       wxImage:destroy(TmpImg),
		       BM
		   end || {X,Y,W,H} <- Areas ],
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
    frame_paint_offset_aligned(Point,R,NewFrames);
frame_paint_offset_aligned({_,YRef}=Point,[{centered_vertically,MidY}|R],Frames) ->
    NewFrames = lists:map(
		  fun(#frame{bitmap = BitMap} = Frame) ->
			  Height = wxBitmap:getHeight(BitMap),
			  Diff = round(MidY - (YRef + Height/2)),
			  Frame#frame{y_paint_offset = Diff}
		  end,Frames),
    frame_paint_offset_aligned(Point,R,NewFrames).

			  

-spec(resize_keep_ratio(enlarge,{factor,integer()},[#frame{}]) -> ok).
resize_keep_ratio(enlarge,{factor,N},Frames) ->
    lists:foreach(
      fun(#frame{bitmap = BitMap}) ->
	      Height = wxBitmap:getHeight(BitMap),
	      Width = wxBitmap:getWidth(BitMap),
	      Ratio = Height / Width,
	      NewHeight = Height * N,
	      NewWidth = NewHeight / Ratio,
	      wxBitmap:setHeight(BitMap,round(NewHeight)),
	      wxBitmap:setWidth(BitMap,round(NewWidth))
      end,Frames),
    ok.

-spec(rotate_frames([#frame{}],{integer(),integer()},float(),sequence|fixed) -> [#frame{}]).
rotate_frames(Frames,CoR,Radians,Mode) ->
    lists:map(
      fun({Frame,SeqInt}) ->
	      #frame{bitmap = BitMap} = Frame,
	      WxImg = wxBitmap:convertToImage(BitMap),
	      wxBitmap:destroy(BitMap),
	      CalcRadians = case Mode of
				fixed ->
				    Radians;
				sequence ->
				    Radians*SeqInt
			    end,
	      TMPImg = wxImage:rotate(WxImg,CalcRadians,CoR),
	      Res = wxBitmap:new(TMPImg),
	      wxImage:destroy(WxImg),
	      Frame#frame{bitmap = Res}
      end,
      lists:zip(Frames,lists:seq(1,length(Frames)))).
		      
		      
