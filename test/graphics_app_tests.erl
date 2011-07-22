-module(graphics_app_tests).
-include_lib("eunit/include/eunit.hrl").
-include("animation.hrl").
-define(START_ANIMATION,[{5,5,2,152},
			 {20,21,4,137},
			 {34,37,8,120},
			 {50,53,8,104},
			 {66,69,8,88},
			 {83,85,8,72},
			 {99,101,8,56},
			 {116,109,10,48},
			 {135,114,16,40},
			 {158,124,30,32},
			 {196,132,32,24},
			 {234,132,36,22},
			 {281,130,48,25},
			 {336,122,63,27},
			 {402,122,64,31},
			 {473,130,64,31},					     
			 {544,132,65,30},
			 {7,205,33,24},
			 {53,199,30,31},
			 {89,194,29,36},
			 {124,187,25,44},
			 {165,181,24,49},
			 {193,181,24,48},
			 {228,179,24,49},
			 {261,178,24,49},
			 {297,179,24,49},
			 {329,179,24,49},
			 {363,180,24,49},
			 {398,181,24,48},
			 {436,185,29,44},
			 {470,189,31,40},
			 {510,192,35,38},
			 {7,247,32,41}
			]).

basic_empty_starts_test_() ->
    {timeout,
     10,
     fun() ->
	     start_graphics("Blank Demo",{200,100},{0,0},10),
	     timer:sleep(3000),
	     stop_graphics()
     end}.

basic_adds_to_paint_screen_removes_test_() ->
    {timeout,10,
     fun() ->
	     start_graphics("Add-Remove from Paint Screen",{400,60},{0,0},10),
	     WxEnv = view_screen:get_wxenv(),
	     wx:set_env(WxEnv),
	     [Ouch_Man] = sprite_lib:get_frames(filename:join([code:priv_dir(graphics),
							       "Misc","megaman.gif"]),
						[{15,560,46,46}]),
	     paint_screen:add_to_paint_screen(1,ouch1,{0,0},Ouch_Man#frame.bitmap),
	     paint_screen:add_to_paint_screen(1,ouch2,{46,0},Ouch_Man#frame.bitmap),
	     paint_screen:add_to_paint_screen(1,ouch3,{46*2,0},Ouch_Man#frame.bitmap),
	     paint_screen:add_to_paint_screen(1,ouch4,{46*3,0},Ouch_Man#frame.bitmap),
	     timer:sleep(2000),
	     paint_screen:remove_from_paint_screen(1,ouch1),
	     paint_screen:remove_from_paint_screen(1,ouch3),
	     timer:sleep(2000),	     
	     stop_graphics()
     end
    }.

basic_background_with_view_screen_move_test_() ->
    {timeout,
     10,
     fun() ->
	     start_graphics("Moving View Screen over Megaman Background",
			    {400,200},{100,200},10),
	     background_entity:add_to_graphics(),
	     Dirs = [up,down,right,left],
	     Move = fun() -> lists:nth(random:uniform(length(Dirs)),Dirs) end,
	     Moves = [Move()|| _ <- lists:seq(1,10)],
	     Prolonged = lists:flatten([string:copies([M],5)||M<-Moves]),
	     lists:map(
	       fun(Direction) ->
		       view_screen:move(Direction),
		       timer:sleep(100)
	       end,Prolonged),
	     stop_graphics()
     end}.

basic_moving_entity_fixed_view_screen_test_() ->    
    {timeout,
     10,
     fun() ->
	     start_graphics("Fixed View Screen - Moving Entities on Paint Screen",
			   {500,150},{0,0},10),
	     human_entity:add_to_graphics(h1),
	     human_entity:add_to_graphics(h2),
	     human_entity:add_to_graphics(h3),
	     human_entity:add_to_graphics(h4),
	     human_entity:add_to_graphics(h5),
	     human_entity:wander(h1,{0,500}),
	     human_entity:wander(h2,{0,500}),
	     human_entity:wander(h3,{0,500}),	     
	     human_entity:wander(h4,{0,500}), 
	     human_entity:wander(h5,{0,500}), 
	     timer:sleep(6000),
	     stop_graphics()
     end}.

rendering_offset_to_put_base_of_frames_on_same_spot_test_() ->
    {timeout,
     30,
     fun() ->
	     start_graphics("Sprite offset",{200,120},{0,0},10),
	     WxEnv = view_screen:get_wxenv(),
	     wx:set_env(WxEnv),
	     Frames = sprite_lib:get_frames(filename:join([code:priv_dir(graphics),
							   "Misc","megaman.gif"]),
					    ?START_ANIMATION),
	     PointX = 50,
	     PointY = 0,
	     ReferencePoint = {PointX,PointY},
	     Adjusted = sprite_lib:frame_paint_offset_aligned(ReferencePoint,
							      [{bottom,100},
							       {centered_horizontally,PointX}],
							      Frames),
	     lists:foreach(
	       fun(Frame) ->
		       #frame{bitmap = BitMap,
			      x_paint_offset = XpO,
			      y_paint_offset = YpO} = Frame,
		       NewPoint = {PointX + XpO, PointY+YpO},
		       paint_screen:add_to_paint_screen(1,coming_in,NewPoint,BitMap),
		       timer:sleep(80)
	       end,Adjusted),
	     timer:sleep(4000),
	     stop_graphics()
     end}.

resize_sprites_test_() ->
    {timeout,
     30,
     fun() ->
	     start_graphics("Offset + Scaled x 3",{200,200},{0,0},10),

	     WxEnv = view_screen:get_wxenv(),
	     wx:set_env(WxEnv),
	     Frames = sprite_lib:get_frames(filename:join([code:priv_dir(graphics),
							   "Misc","megaman.gif"]),
					    ?START_ANIMATION),

	     sprite_lib:resize_keep_ratio(enlarge,{factor,3},Frames),

	     PointX = 50,
	     PointY = 0,
	     ReferencePoint = {PointX,PointY},
	     Adjusted = sprite_lib:frame_paint_offset_aligned(ReferencePoint,
							      [{bottom,180},
							       {centered_horizontally,PointX}],
							      Frames),
	     lists:foreach(
	       fun(Frame) ->
		       #frame{bitmap = BitMap,
			      x_paint_offset = XpO,
			      y_paint_offset = YpO} = Frame,
		       NewPoint = {PointX + XpO, PointY+YpO},
				     paint_screen:add_to_paint_screen(1,coming_in,NewPoint,BitMap),
		       timer:sleep(80)
	       end,Adjusted),
	     timer:sleep(4000),
	     stop_graphics()
     end}.

start_graphics(Title,Dimensions,ViewScreenPosition,PaintScreenPause) ->
    application:load(graphics),
    application:set_env(graphics,view_screen_title,Title),
    application:set_env(graphics,view_screen_size,Dimensions),
    application:set_env(graphics,view_screen_pos,ViewScreenPosition),
    application:set_env(graphics,paint_screen_update_pause,PaintScreenPause),
    application:start(graphics).

stop_graphics() ->
    application:stop(graphics),
    application:unload(graphics).

