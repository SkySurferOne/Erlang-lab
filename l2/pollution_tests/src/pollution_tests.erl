%%%-------------------------------------------------------------------
%%% @author damian
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. kwi 2017 11:04
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("damian").

-include_lib("eunit/include/eunit.hrl").

%% imports
-import(pollution, [createMonitor/0, addStation/3,
  addValue/5, getOneValue/4, getDeviation/3]).

simple_test() ->
  ?assert(true).

getOneValue_test() ->
  P = createMonitor(),
  P2 = addStation('Station 1', {52, 32}, P),
  P3 = addStation('Station 2', {55, 32}, P2),
  P4 = addValue('Station 1', {{2017,5,4},{21,22,39}}, 'PM2', 10.0, P3),
  ?assertEqual([10.0], getOneValue('Station 1', {2017,5,4}, 'PM2', P4)),
  ?assertEqual([], getOneValue('Station 2', {2017,5,4}, 'PM2', P4)).

pow(X) -> X * X.

getDeviation_test() ->
  P = createMonitor(),
  P2 = addStation('Station 1', {52, 32}, P),
  P4 = addValue('Station 1', {{2017,5,4},{21,22,39}}, 'PM2', 10.0, P2),
  P5 = addValue('Station 1', {{2017,5,4},{21,22,39}}, 'PM2', 20.0, P4),
  P6 = addValue('Station 1', {{2017,5,4},{21,22,39}}, 'PM2', 30.0, P5),
  ?assertEqual(10.0, getDeviation('PM2', {21,22,39}, P6)),
  ?assertError(badarith, getDeviation('PM1', {21,22,39}, P6)).

%% goo.gl/kxShFQ
%% test 5 funkcja
