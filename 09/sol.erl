-module(sol).
-compile(export_all).

parse([]) ->
	[];
parse([Line | Rest]) ->
	[X,Y] = [list_to_integer(N) || N <- string:split(Line, ",", all)],
	[{X,Y} | parse(Rest)].

canonical({{Xa,Ya}, {Xb,Yb}}) ->
	{{min(Xa,Xb), min(Ya,Yb)}, {max(Xa,Xb), max(Ya,Yb)}}.
rect_size({{X1,Y1}, {X2,Y2}}) ->
	(X2 - X1 + 1) * (Y2 - Y1 + 1).

max_size(Tiles) -> max_size(Tiles, 0).
max_size([], Max) ->
	Max;
max_size([Tile | Rest], Max) ->
	Max1 = lists:foldl(fun(Tile2, Max0) ->
		Size = rect_size(canonical({Tile,Tile2})),
		max(Max0, Size)
	end, Max, Rest),
	max_size(Rest, Max1).
