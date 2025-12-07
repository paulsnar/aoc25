-module(sol).
-compile(export_all).

parse(Lines) ->
	parse_start([list_to_binary(X) || X <- Lines]).
parse_start([Start | Lines]) ->
	{X, 1} = binary:match(Start, <<"S">>),
	Splitters = parse_splitters(Lines, 1),
	{map, X, Splitters}.

parse_splitters([], _) ->
	[];
parse_splitters([Line | Rest], Y) ->
	Pos = [X || {X, 1} <- binary:matches(Line, <<"^">>)],
	[sets:from_list(Pos) | parse_splitters(Rest, Y + 1)].

sol1({map, StartX, Splitters}) ->
	eval(sets:from_list([StartX]), Splitters, 0).
eval(_, [], SplitCount) ->
	SplitCount;
eval(Beams0, [Splitters | Rest], SplitCount0) ->
	{Beams1, SplitCount1} = sets:fold(fun(SplX, {B, SC}) ->
		case sets:is_element(SplX, Beams0) of
			false -> {B, SC};
			true ->
				B1 = support:pipe([
					fun(Bb) -> sets:del_element(SplX, Bb) end,
					fun(Bb) -> sets:add_element(SplX - 1, Bb) end,
					fun(Bb) -> sets:add_element(SplX + 1, Bb) end
				], B),
				{B1, SC + 1}
		end
	end, {Beams0, SplitCount0}, Splitters),
	eval(Beams1, Rest, SplitCount1).

sol2({map, StartX, Splitters}) ->
	eval2(#{StartX => 1}, Splitters).

eval2_split(Sup, X, Num) ->
	support:pipe([
		fun(S) -> maps:remove(X, S) end,
		fun(S) -> maps:update_with(X - 1, fun(N) -> N + Num end, Num, S) end,
		fun(S) -> maps:update_with(X + 1, fun(N) -> N + Num end, Num, S) end
	], Sup).
eval2_step(Sup0, Splitters) ->
	sets:fold(fun (X, Sup) ->
		case maps:get(X, Sup, nil) of
			nil -> Sup;
			Num -> eval2_split(Sup, X, Num)
		end
	end, Sup0, Splitters).
eval2(Sup, []) ->
	lists:sum(maps:values(Sup));
eval2(Sup0, [Splitters | Rest]) ->
	Sup1 = eval2_step(Sup0, Splitters),
	eval2(Sup1, Rest).
