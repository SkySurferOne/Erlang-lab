-module(onp).
-import(string,[tokens/2, str/2]).
-import(math,[sqrt/1, sin/1, cos/1, tan/1, pow/2]).
-export([eval/1]).

parse_to_float(Str) ->
  case str(Str, ".") of
    0 -> float(list_to_integer(Str));
    _ -> list_to_float(Str)
  end.

eval(Expr) ->
  eval(tokens(Expr, " "), []).

eval([], [R]) -> R;
eval(["+"|T], [A,B|Stack]) ->
  eval(T, [B+A|Stack]);
eval(["-"|T], [A,B|Stack]) ->
  eval(T, [B-A|Stack]);
eval(["*"|T], [A,B|Stack]) ->
  eval(T, [B*A|Stack]);
eval(["/"|T], [A,B|Stack]) ->
  case A /= 0.0 of
    true -> eval(T, [B/A|Stack]);
    false -> error(badarith)
  end;
eval(["sqrt"|T], [A|Stack]) ->
  eval(T, [sqrt(A)|Stack]);
eval(["sin"|T], [A|Stack]) ->
  eval(T, [sin(A)|Stack]);
eval(["cos"|T], [A|Stack]) ->
  eval(T, [cos(A)|Stack]);
eval(["tan"|T], [A|Stack]) ->
  eval(T, [tan(A)|Stack]);
eval(["pow"|T], [A,B|Stack]) ->
  eval(T, [pow(B,A)|Stack]);
eval([H|T], Stack) ->
  eval(T, [parse_to_float(H)|Stack]).

