-module(qsort).
-export([qs/1, randomElems/3, compareSpeeds/3]).
-import(random,[uniform/1]).
-import(lists,[seq/2]).
-import(timer,[tc/2]).
-import(io,[format/2]).

lessThan(List, Arg)
  -> [X || X <- List, X < Arg].

 grtEqThan(List, Arg) ->
   [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot|Tail]) ->
  qs( lessThan(Tail, Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail, Pivot) ).

randomElems(N, Min, Max) ->
  [uniform(Max - Min) + Min || B <- seq(1, N)].

compareSpeeds(List, Fun1, Fun2)
  -> [tc(Fun1, [List]), tc(Fun2, [List])].
