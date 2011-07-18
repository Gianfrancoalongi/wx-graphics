%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2011, Gianfranco
%%% Created : 17 Jul 2011 by Gianfranco <zenon@zen.local>
-module(entity).
-export([start_link/3]).
start_link(WxEnv,Module,{SpritePath,Id}) ->
    Module:start_link(WxEnv,{SpritePath,Id}).
