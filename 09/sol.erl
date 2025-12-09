-module(sol).
-compile(export_all).

parse([]) ->
	[];
parse([Line | Rest]) ->
	[X,Y] = [list_to_integer(N) || N <- string:split(Line, ",", all)],
	[{X,Y} | parse(Rest)].

rect_size({{X1,Y1}, {X2,Y2}}) ->
	(abs(X2 - X1) + 1) * (abs(Y2 - Y1) + 1);
rect_size({X1,Y1,X2,Y2}) ->
	(X2 - X1 + 1) * (Y2 - Y1 + 1).
rect_canonical({{X1,Y1},{X2,Y2}}) ->
	{min(X1,X2), min(Y1,Y2), max(X1,X2), max(Y1,Y2)}.

sol1(Tiles) -> max_size(Tiles, 0).
max_size([], Max) ->
	Max;
max_size([Tile1 | Rest], Max) ->
	Max1 = lists:foldl(fun(Tile2, Max0) ->
		Size = rect_size({Tile1,Tile2}),
		max(Max0, Size)
	end, Max, Rest),
	max_size(Rest, Max1).

edges(Tiles) ->
	TilesRot = tl(Tiles) ++ [hd(Tiles)],
	[{min(X1,X2),max(X1,X2),min(Y1,Y2),max(Y1,Y2)}
		|| {{X1,Y1},{X2,Y2}} <- lists:zip(Tiles, TilesRot)].

edge_overlaps({_,X2,_,_}, {Xa,_,_,_}) when X2 =< Xa -> false;
edge_overlaps({X1,_,_,_}, {_,_,Xz,_}) when X1 >= Xz -> false;
edge_overlaps({_,_,_,Y2}, {_,Ya,_,_}) when Y2 =< Ya -> false;
edge_overlaps({_,_,Y1,_}, {_,_,_,Yb}) when Y1 >= Yb -> false;
edge_overlaps(_,_) -> true.

sol2(Tiles) -> max_size_inner(Tiles, edges(Tiles), 0).
max_size_inner([], _, Max) ->
	Max;
max_size_inner([Tile1 | Rest], Edges, Max0) ->
	Max1 = lists:foldl(fun(Tile2, Max) ->
		Rect = rect_canonical({Tile1,Tile2}),
		case lists:any(fun(Edge) -> edge_overlaps(Edge, Rect) end, Edges) of
			true -> Max;
			false -> max(Max, rect_size(Rect))
		end
	end, Max0, Rest),
	max_size_inner(Rest, Edges, Max1).

