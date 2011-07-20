-module(graphics_app_tests).
-include_lib("eunit/include/eunit.hrl").
-include("animation.hrl").

basic_empty_starts_test_() ->
    {timeout,
     10,
     fun() ->
	     application:load(graphics),
	     application:set_env(graphics,view_screen_title,"Blank Demo"),
	     application:set_env(graphics,view_screen_size,{200,100}),
	     application:set_env(graphics,view_screen_pos,{0,0}),
	     application:set_env(graphics,paint_screen_update_pause,10),
	     application:start(graphics),
	     timer:sleep(3000),
	     application:stop(graphics),
	     application:unload(graphics)
     end}.

basic_adds_to_paint_screen_removes_test_() ->
    {timeout,10,
     fun() ->
	     application:load(graphics),
	     application:set_env(graphics,view_screen_title,"Add-Remove from Paint Screen"),
	     application:set_env(graphics,view_screen_size,{400,60}),
	     application:set_env(graphics,view_screen_pos,{0,0}),
	     application:set_env(graphics,paint_screen_update_pause,10),
	     application:start(graphics),
	     
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
	     application:stop(graphics),
	     application:unload(graphics)
     end
    }.

basic_background_with_view_screen_move_test_() ->
    {timeout,
     10,
     fun() ->
	     application:load(graphics),
	     application:set_env(graphics,view_screen_title,"Moving View Screen over Megaman Background"),
	     application:set_env(graphics,view_screen_size,{400,100}),
	     application:set_env(graphics,view_screen_pos,{100,200}),
	     application:set_env(graphics,paint_screen_update_pause,10),
	     application:start(graphics),
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
	     application:stop(graphics),
	     application:unload(graphics)
     end}.

basic_moving_entity_fixed_view_screen_test_() ->    
    {timeout,
     10,
     fun() ->
	     application:load(graphics),
	     application:set_env(graphics,view_screen_title,"Fixed View Screen - Moving Entities on Paint Screen"),
	     application:set_env(graphics,view_screen_size,{500,150}),
	     application:set_env(graphics,view_screen_pos,{0,0}),
	     application:set_env(graphics,paint_screen_update_pause,10),
	     application:start(graphics),
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
	     application:stop(graphics),
	     application:unload(graphics)
     end}.

%% base_of_sprite_frame_is_at_fix_position_with_rendering_offset_test_() ->
%%     {timeout,
%%      10,
%%      fun() ->
%% 	     application:load(graphics),
%% 	     application:set_env(graphics,view_screen_title,"Base of sprite is offset by rendering offset"),
%% 	     application:set_env(graphics,view_screen_size,{500,150}),
%% 	     application:set_env(graphics,view_screen_pos,{0,0}),
%% 	     application:set_env(graphics,paint_screen_update_pause,10),
%% 	     application:start(graphics),
	     
%% 	     WxEnv = view_screen:get_wxenv(),
%% 	     wx:set_env(WxEnv),
%% 	     BitMaps= sprite_lib:get_frames(filename:join([code:priv_dir(graphics),
%% 							   "Misc","megaman.gif"]),
%% 					    [{4,4,6,155},
%% 					     {17,17,10,138},
%% 					     {35,35,10,120},
%% 					     {48,52,10,106},
%% 					     {67,67,10,88},
%% 					     {83,83,10,73},
%% 					     {100,100,10,56},
%% 					     {114,107,12,50},
%% 					     {134,116,18,40},
%% 					     {157,124,32,34},
%% 					     {196,130,32,35}]),

%% 	     Animation = sprite_lib:align_frames({bottom,150},
	     

%% 	     timer:sleep(6000),
%% 	     application:stop(graphics),
%% 	     application:unload(graphics)
%%      end}.	     

    


