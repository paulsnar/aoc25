-module(sol).
-compile(export_all).

-record(dial, {value, password=0}).

sol1(Instr) ->
	D = lists:foldl(fun update1/2, #dial{value=50},
		[parse_instr(X) || X <- Instr]),
	D#dial.password.

parse_instr("L" ++ Num) -> -list_to_integer(Num);
parse_instr("R" ++ Num) -> list_to_integer(Num).

update1(Instr, D = #dial{value=Value, password=Pw}) ->
	Value1 = clamp1(Value + Instr),
	case Value1 of
		0 -> #dial{value=Value1, password=Pw + 1};
		_ -> D#dial{value=Value1}
	end.

clamp1(X) when X < 0 -> clamp1(X + 100);
clamp1(X) when X >= 100 -> clamp1(X - 100);
clamp1(X) -> X.
