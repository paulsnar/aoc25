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

% sanity check assert
eval2(Value, _, _) when Value < 0 -> error("negative value not allowed");

% special case around 0 – on leftward rotation that lands on exactly 0, treat
% initial reaching 0 as the password increment but NOT the rotation away from
% 0 leftwards
eval2(Value, Instr, Rot) when Value + Instr == 0 -> {0, Rot + 1};
eval2(0, Instr, Rot) when Instr < 0 -> eval2(99, Instr + 1, Rot);

% left/right turns within range without crossing zero boundary
eval2(Value, Instr, Rot)
	when Value + Instr > 0, Value + Instr < 100 ->
	{Value + Instr, Rot};

% overfull turns
eval2(Value, Instr, Rot) when Instr div 100 /= 0 ->
	Rots = abs(Instr div 100),
	Rem = Instr rem 100,
	eval2(Value, Rem, Rot + Rots);

% overflowing rightward turns
eval2(Value, Instr, Rot) when Instr + Value >= 100 ->
	Rots = (Instr + Value) div 100,
	Rem = (Instr + Value) rem 100,
	{Rem, Rot + Rots};
% overflowing leftward turns
eval2(Value, Instr, Rot) when Instr + Value < 0 ->
	Rots = (Instr + Value) div 100,
	Rem = (Instr + Value) rem 100,
	{100 + Rem, Rot + abs(Rots) + 1};

eval2(Value, Instr, Rot) when Instr < 0 ->
	{Value + 100 + Instr, Rot};
eval2(Value, Instr, Rot) when Instr < 100, Value + Instr < 100 ->
	{Value + Instr, Rot}.
