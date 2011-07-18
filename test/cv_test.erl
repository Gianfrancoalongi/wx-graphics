-module(cv_test).
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    {foreach,
     fun() -> application:start(commit) end,
     fun(_) -> application:stop(commit) end,
     [
      fun application_starts/0,
      fun add_background/0
     ]}.


application_starts() ->
    ok.

add_background() ->
    entity_sup:start_entity(background_entity,[bg]).
