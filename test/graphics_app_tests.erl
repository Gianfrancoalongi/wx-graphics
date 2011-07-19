-module(graphics_app_tests).
-include_lib("eunit/include/eunit.hrl").


basic_empty_starts_test_() ->
    {timeout,
     10,
     fun() ->
	     application:start(sasl),
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

