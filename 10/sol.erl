-module(sol).
-compile(export_all).

-record(machine, {lights, buttons, joltage}).

parse([]) ->
	[];
parse([Line | Rest]) ->
	[Lights | ButJlt] = string:split(Line, " ", all),
	{Buts, [Jlt]} = lists:split(length(ButJlt) - 1, ButJlt),
	LightsB = parse_lights(Lights),
	ButsB = lists:map(fun parse_but/1, Buts),
	JltB = parse_jlt(Jlt),
	[#machine{lights=LightsB, buttons=ButsB, joltage=JltB}
		| parse(Rest)].

parse_lights("[" ++ Lights) ->
	"]" ++ LightsR = lists:reverse(Lights),
	parse_lights(LightsR, 0).
parse_lights("#" ++ Rest, L) ->
	parse_lights(Rest, (L bsl 1) bor 1);
parse_lights("." ++ Rest, L) ->
	parse_lights(Rest, L bsl 1);
parse_lights("", L) ->
	L.

parse_but(But) ->
	Nums = [list_to_integer(X)
		|| X <- string:split(string:trim(But, both, "()"), ",", all)],
	lists:foldl(fun(Pos, B) ->
		B bor (1 bsl Pos)
	end, 0, Nums).

parse_jlt(Jlt) ->
	[list_to_integer(X)
		|| X <- string:split(string:trim(Jlt, both, "{}"), ",", all)].

eval(Mach, State0, Step) ->
	Q0 = eval_start(Mach, State0, Step),
	eval_do(Q0).
eval_do(Q0) ->
	case eval_step(Q0) of
		{ok, Depth, Btns} ->
			{ok, Depth, Btns};
		{next, Q1} ->
			eval_do(Q1)
	end.
eval_step(Q0) ->
	{{value, R}, Q1} = queue:out(Q0),
	case R() of
		{ok, Depth, Btns} ->
			{ok, Depth, Btns};
		{next, Rs} ->
			Q2 = lists:foldl(fun queue:in/2, Q1, Rs),
			{next, Q2}
	end.
eval_start(Mach, State0, Step) ->
	Start = eval_next(1, State0, Step, Mach, []),
	lists:foldl(fun queue:in/2, queue:new(), Start).
eval_next(Depth, State, Step, Mach=#machine{buttons=Btns}, History) ->
	[fun() -> eval_step(Depth, State, B, Step, Mach, History) end
		|| B <- Btns].
eval_step(Depth, State, Button, Step, Mach, History) ->
	case Step(State, Button, Mach) of
		true -> {ok, Depth, lists:reverse([Button | History])};
		{false, State1} ->
			H1 = [Button | History],
			N = eval_next(Depth + 1, State1, Step, Mach, H1),
			{next, N};
		dead ->
			{next, []}
	end.

eval1_step(State, Button, #machine{lights=WantedState}) ->
	State1 = State bxor Button,
	case State1 =:= WantedState of
		true ->
			true;
		false ->
			{false, State1}
	end.

sol1(Machs) ->
	Depths = lists:map(fun(Mach) ->
		{ok, Depth, _} = eval(Mach, 0, fun eval1_step/3),
		Depth
	end, Machs),
	lists:sum(Depths).

eval2_inc(J, 0) ->
	J;
eval2_inc([Val | Rest], N) ->
	Val1 = case (N band 1) of
		1 -> Val + 1;
		0 -> Val
	end,
	[Val1 | eval2_inc(Rest, N bsr 1)].
eval2_step(J0, Button, #machine{joltage=WantedJ}) ->
	J1 = eval2_inc(J0, Button),
	case J1 =:= WantedJ of
		true ->
			true;
		false ->
			case J1 > WantedJ of
				true -> dead;
				false -> {false, J1}
			end
	end.

sol2_mach(Mach) ->
	J0 = [0 || _ <- Mach#machine.joltage],
	eval(Mach, J0, fun eval2_step/3).
