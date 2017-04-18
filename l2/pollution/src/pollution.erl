%%%-------------------------------------------------------------------
%%% @author damian
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. kwi 2017 15:22
%%%-------------------------------------------------------------------
-module(pollution).
-author("damian").

%% imports
-import(lists, [filter/2, map/2]).

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4,
  getOneValue/4, getStationMean/3, getDailyMean/3, getDeviation/2, test/0]).


-record(station, {coords, measurements = []}).
-record(measure, {measureType, datetime, value}).
-define(GetValueFromDict(Key, Dict),
  lists:nth(1, element(2, dict:find(StationName, Monitor)))).
-define(AddMeasureToStation(StationRecord, DateTime, MeasureType, Value),
  StationRecord#station{measurements = lists:append(
    [#measure{measureType = MeasureType, datetime = DateTime, value = Value}], StationRecord#station.measurements)}).
-define(ReplaceMeasurements(StationRecord, Measurements),
  StationRecord#station{measurements = Measurements}).
-define(UpdateDict(StationName, NewStationRecord, Dict),
  dict:append(StationName, NewStationRecord, dict:erase(StationName, Monitor))).
-define(FilterMeasurements(StationRecord, Fun),
  lists:filter(fun(R) ->
    Fun(element(1, R#measure.datetime), R#measure.measureType) end, StationRecord#station.measurements)).
-define(GetKeys(Dict), lists:map(fun (T) -> element(1, T) end, dict:to_list(Dict))).

% helper functions
avr(List) -> avr(0, 0, List).
avr(0, _, []) -> error(badarith);
avr(C, Sum, []) -> Sum / C;
avr(C, Sum, [H|T]) -> avr(C+1, Sum+H, T).

date(Datetime) -> element(1, Datetime).
time(Datetime) -> element(2, Datetime).

%% API implementation
createMonitor() -> dict:new().
addStation(StationName, Coords, Monitor) ->
  case dict:is_key(StationName, Monitor) of
    false -> dict:append(StationName, #station{coords = Coords}, Monitor);
    true -> error(badarg)
  end.

addValue(StationName, DateTime, MeasureType, Value, Monitor) ->
  StationRecord = ?GetValueFromDict(StationName, Monitor),
  StationRecordMod = ?AddMeasureToStation(StationRecord, DateTime, MeasureType, Value),
  ?UpdateDict(StationName, StationRecordMod, Monitor).

removeValue(StationName, Date, MeasureType, Monitor) ->
  StationRecord = ?GetValueFromDict(StationName, Monitor),
  MeasurementsMod = ?FilterMeasurements(StationRecord, fun(ThatDate, ThatMeasureType) ->
    (Date /= ThatDate) and (MeasureType /= ThatMeasureType) end),
  ?UpdateDict(StationName, ?ReplaceMeasurements(StationRecord, MeasurementsMod), Monitor).

getOneValue(StationName, Date, MeasureType, Monitor) ->
  StationRecord = ?GetValueFromDict(StationName, Monitor),
  MeasurementsMod = ?FilterMeasurements(StationRecord, fun(ThatDate, ThatMeasureType) ->
    (Date == ThatDate) and (MeasureType == ThatMeasureType) end),
  lists:map(fun (R) -> R#measure.value end, MeasurementsMod).

getStationMean(StationName, MeasureType, Monitor) ->
  StationRecord = ?GetValueFromDict(StationName, Monitor),
  MeasurementsMod = ?FilterMeasurements(StationRecord, fun(_, ThatMeasureType) ->
    (MeasureType == ThatMeasureType) end),
  Values = lists:map(fun (R) -> R#measure.value end, MeasurementsMod),
  avr(Values).

getDailyMean(Date, MeasureType, Monitor) ->
  StationNames = ?GetKeys(Monitor),
  Values = lists:flatmap(fun (StationName) -> getOneValue(StationName, Date, MeasureType, Monitor) end, StationNames),
  io:fwrite("Val ~62p~n", [Values]),
  avr(Values).

getDeviation(MeasureType, Time) -> null.

test() ->
  P = createMonitor(),
  P2 = addStation('Station 1', {52, 32}, P),
  P3 = addStation('Station 2', {55, 32}, P2),
  Datetime = calendar:local_time(),
  P4 = addValue('Station 1', Datetime, 'PM2', 10.0, P3),
  P5 = addValue('Station 1', Datetime, 'PM2', 20.0, P4),
  P6 = addValue('Station 1', Datetime, 'PM2', 30.0, P5),
  P7 = addValue('Station 2', Datetime, 'PM2', 30.0, P6),
  io:fwrite("~62p~n", [P7]),
  Values = getOneValue('Station 1', date(Datetime), 'PM2', P7),
  io:fwrite("~62p~n", [Values]),
  Avr = getStationMean('Station 1', 'PM2', P7),
  io:fwrite("~62p~n", [Avr]),
  Avr2 = getDailyMean(date(Datetime), 'PM2', P7),
  io:fwrite("~62p~n", [Avr2]),
  P8 = removeValue('Station 1', date(Datetime), 'PM2', P7),
  io:fwrite("~62p~n", [P8]).
