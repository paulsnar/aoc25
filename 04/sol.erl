-module(sol).
-compile(export_all).

parse(Lines) ->
	parse_lines(Lines, 0, #{}).
parse_lines([], _, Cells) ->
	Cells;
parse_lines([Line | Rest], Y, Cells0) ->
	Cells1 = parse_line(Line, 0, Y, Cells0),
	parse_lines(Rest, Y + 1, Cells1).
parse_line([], _, _, Cells) ->
	Cells;
parse_line([$. | Rest], X, Y, Cells0) ->
	parse_line(Rest, X + 1, Y, Cells0);
parse_line([$@ | Rest], X, Y, Cells0) ->
	parse_line(Rest, X + 1, Y, Cells0#{{X,Y} => roll}).

accessible(Cells, {X, Y}) ->
	lists:sum([
		case is(roll, Cells, Neigh) of
			true -> 1;
			false -> 0
		end
		|| Neigh <- neighbors({X, Y})
	]) < 4.
neighbors({X, Y}) ->
	[
		{X1, Y1}
		|| X1 <- lists:seq(X - 1, X + 1), Y1 <- lists:seq(Y - 1, Y + 1),
			{X, Y} =/= {X1, Y1}
	].

is(Type, Cells, Pos) ->
	Actual = maps:get(Pos, Cells, empty),
	Actual =:= Type.

sol1(Cells) ->
	maps:fold(fun(Pos, roll, Count) ->
		case accessible(Cells, Pos) of
			true -> Count + 1;
			false -> Count
		end
	end, 0, Cells).

clear(Cells) ->
	maps:filter(fun(Pos, roll) ->
		not accessible(Cells, Pos)
	end, Cells).

sol2(Cells0) ->
	Cells1 = support:fixpoint(Cells0, fun clear/1),
	maps:size(Cells0) - maps:size(Cells1).
