-module(sol).
-compile(export_all).

-record(dial, {value, password=0}).

parse(Lines) ->
	[parse_line(Line) || Line <- Lines].
parse_line("L" ++ Num) -> -list_to_integer(Num);
parse_line("R" ++ Num) -> list_to_integer(Num).

sol1(Instr) ->
	D = lists:foldl(fun update1/2, #dial{value=50}, Instr),
	D#dial.password.

update1(Instr, D = #dial{value=Value}) ->
	Value1 = clamp1(Value + Instr),
	case Value1 of
		0 -> #dial{value=Value1, password=D#dial.password + 1};
		_ -> D#dial{value=Value1}
	end.

clamp1(X) when X < 0 -> clamp1(X + 100);
clamp1(X) when X >= 100 -> clamp1(X - 100);
clamp1(X) -> X.

sol2(Instr) ->
	D = lists:foldl(fun update2/2, #dial{value=50}, Instr),
	D#dial.password.

update2(Instr, #dial{value=Value, password=Password}) ->
	{Value1, Rot} = eval2(Value, Instr),
	#dial{value=Value1, password=Password + Rot}.
eval2(Value, Instr) -> eval2(Value, Instr, 0).

% noop
eval2(Value, 0, Rot) ->
	{Value, Rot};

% starting from 0 (preserve rotation number)
eval2(0, Instr, Rot) when Instr > 0 ->
	eval2(1, Instr - 1, Rot);
eval2(0, Instr, Rot) when Instr < 0 ->
	eval2(99, Instr + 1, Rot);

% more than a full turn
eval2(Value, Instr, Rot) when Value + Instr >= 200 ->
	eval2(Value, Instr - 100, Rot + 1);
eval2(Value, Instr, Rot) when Value + Instr =< -100 ->
	eval2(Value, Instr + 100, Rot + 1);

% crossing zero boundary
eval2(Value, Instr, Rot) when Value + Instr >= 100 ->
	{(Value + Instr) rem 100, Rot + 1};
eval2(Value, Instr, Rot) when Value + Instr < 0 ->
	{100 + (Value + Instr) rem 100, Rot + 1};

% lands on exactly 0
eval2(Value, Instr, Rot) when Value + Instr == 0; Value + Instr == 100 ->
	{0, Rot + 1};

% within boundary
eval2(Value, Instr, Rot) when Value + Instr > 0, Value + Instr < 100 ->
	{Value + Instr, Rot}.
