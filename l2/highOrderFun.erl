%%%-------------------------------------------------------------------
%%% @author Damian
%%% @copyright (C) 2017, <AGH>
%%% @doc
%%%
%%% @end
%%% Created : 18. kwi 2017 14:30
%%%-------------------------------------------------------------------
-module(highOrderFun).
-author("damian").

% imports
-import(lists, [foldl/3]).

%% API
-export([map/2, filter/2, countList/1]).

map(_, []) -> [];
map(Fun, [H|T]) ->
  [Fun(H)] ++ map(Fun, T).

filter(_, []) -> [];
filter(Fun, [H|T]) ->
  case Fun(H) of
    true -> [H] ++ filter(Fun, T);
    false -> filter(Fun, T)
  end.

countList(List) ->
  lists:foldl(fun(_, Y) -> 1+Y end, 0, List).