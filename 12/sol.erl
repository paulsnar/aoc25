-module(sol).
-compile(export_all).

parse(Lines) ->
	{Shapes, Rest} = parse_shapes(Lines),
	Spaces = parse_spaces(Rest),
	{Shapes, Spaces}.

parse_shapes([_Index, L1, L2, L3, "" | Rest]) ->
	Shape = L1 ++ L2 ++ L3,
	Next = hd(Rest),
	case re:run(Next, "^\\d:$") of
		{match, _} ->
			{NextShapes, RestOut} = parse_shapes(Rest),
			{[Shape | NextShapes], RestOut};
		nomatch -> {[Shape], Rest}
	end.

parse_spaces([]) ->
	[];
parse_spaces([Line | Rest]) ->
	{match, [_ | ParamsRaw]} = re:run(Line,
		"^(\\d+)x(\\d+): (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+)$",
		[{capture,all,list}]),
	[X, Y | Ps] = lists:map(fun erlang:list_to_integer/1, ParamsRaw),
	Space = {X,Y, Ps},
	[Space | parse_spaces(Rest)].

check_fits({X,Y,Ps}, Shapes) ->
	Naive = check_fits_naive(X, Y, lists:sum(Ps)),
	Tiled = check_fits_tiled(X, Y, lists:zip(Ps, Shapes)),
	case {Naive, Tiled} of
		{true, _} -> true;
		{_, false} -> false;
		_ -> unknown
	end.
check_fits_naive(X, Y, TotalPresents) ->
	Tiles = (X div 3) * (Y div 3),
	Tiles >= TotalPresents.
check_fits_tiled(X, Y, PsShapes) ->
	Size = X * Y,
	NeededSize = lists:sum(lists:map(fun({Num, Shape}) ->
		Num * support:count(fun($#) -> true; (_) -> false end, Shape)
	end, PsShapes)),
	Size >= NeededSize.

sol1({Shapes, Spaces}) ->
	Inc = fun(X) -> X + 1 end,
	lists:foldl(fun(Space, M) ->
		Fits = check_fits(Space, Shapes),
		maps:update_with(Fits, Inc, M)
	end, #{true => 0, false => 0, unknown => 0}, Spaces).
