%%%-------------------------------------------------------------------
%%% @author damian
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. maj 2017 10:10
%%%-------------------------------------------------------------------
-module(pollution_server_basic).
-author("damian").

%% imports
-import(pollution, [createMonitor/0, addStation/3, addValue/5, removeValue/4,
getOneValue/4, getStationMean/3, getDailyMean/3, getDeviation/3]).

%% API
-export([start/0, init/0, loop/1, terminate/0, addStation/2, addValue/4, printMonitor/0]).

start() ->
  register(pollution_server, spawn(pollution_server_basic, init, [])).

init() ->
  Monitor = createMonitor(),
  loop(Monitor).

loop(Monitor) ->
  receive
    {msg, Msg} ->
      io:fwrite("~62p~n", [Msg]),
      loop(Monitor);

    {addStation, Pid, StationName, Coords} ->
      case pollution:addStation(StationName, Coords, Monitor) of
        {error, Msg} -> Pid ! {msg, Msg},
          loop(Monitor);
        NewMonitor -> Pid ! {msg, ok},
            loop(NewMonitor)
      end;

    {addValue, Pid, StationName, DateTime, MeasureType, Value} ->
      case pollution:addValue(StationName, DateTime, MeasureType, Value, Monitor) of
        {error, Msg} -> Pid ! {msg, Msg},
          loop(Monitor);
        NewMonitor -> Pid ! {msg, ok},
            loop(NewMonitor)
      end;

    {printMonitor} ->
      io:fwrite("~62p~n", [Monitor]),
      loop(Monitor);

    stop ->
      terminate()
  end.

terminate() ->
  ok.

addStation(StationName, Coords) ->
  pollution_server ! {addStation, self(), StationName, Coords}.

addValue(StationName, DateTime, MeasureType, Value) ->
  pollution_server ! {addValue, self(), StationName, DateTime, MeasureType, Value}.

removeValue(StationName, Date, MeasureType) ->
  pollution_server ! {removeValue, self(), StationName, Date, MeasureType}.

getOneValue(StationName, Date, MeasureType) ->
  pollution_server ! {getOneValue, self(), StationName, Date, MeasureType}.

getDailyMean(StationName, MeasureType) ->
  pollution_server ! {getDailyMean, self(), StationName, MeasureType}.

getDeviation(MeasureType, Time) ->
  pollution_server ! {getDeviation, self(), MeasureType, Time}.

%% utils
printMonitor() ->
  pollution_server ! {printMonitor}.

