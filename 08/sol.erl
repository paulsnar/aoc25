-module(sol).
-compile(export_all).

parse([]) ->
	[];
parse([Line | Rest]) ->
	[X,Y,Z] = [list_to_integer(N) || N <- string:split(Line, ",", all)],
	[{X,Y,Z} | parse(Rest)].

distance({Xa,Ya,Za}, {Xb,Yb,Zb}) ->
	math:sqrt(
		math:pow(Xa - Xb, 2) +
		math:pow(Ya - Yb, 2) +
		math:pow(Za - Zb, 2)
	).

distance_matrix(Els) -> distance_matrix(Els, #{}).
distance_matrix([], Matrix) ->
	Matrix;
distance_matrix([El1 | Rest], Matrix) ->
	Matrix1 = lists:foldl(fun(El2, M) ->
		Distance = distance(El1, El2),
		AB = canonical(El1, El2),
		M#{AB => Distance}
	end, Matrix, Rest),
	distance_matrix(Rest, Matrix1).

canonical(A = {Xa,_,_}, B = {Xb,_,_}) when Xa < Xb -> {A, B};
canonical(A = {X,Ya,_}, B = {X,Yb,_}) when Ya < Yb -> {A, B};
canonical(A = {X,Y,Za}, B = {X,Y,Zb}) when Za < Zb -> {A, B};
canonical(A, A) -> {A, A};
canonical(A, B) -> canonical(B, A).

sol1(Els, N) ->
	Matrix = distance_matrix(Els),
	MatrixEntries = lists:sort(fun({_, Da}, {_, Db}) ->
		Da =< Db
	end, maps:to_list(Matrix)),
	Graph = graph_build(Els),
	graph_connect(Graph, [
		Conn
		|| {Conn, _Weight} <- lists:sublist(MatrixEntries, N)
	]),
	Hoods = lists:reverse(lists:sort(graph_circuits(Graph))),
	[A,B,C | _] = Hoods,
	A * B * C.

graph_build(Els) ->
	DG = digraph:new(),
	lists:foreach(fun(Pos) ->
		Pos = digraph:add_vertex(DG, Pos, [])
	end, Els),
	DG.

graph_connect(DG, Conns) ->
	lists:foreach(fun({A,B}) ->
		{A,B} = digraph:add_edge(DG, {A,B}, A, B, []),
		{B,A} = digraph:add_edge(DG, {B,A}, B, A, [])
	end, Conns).

graph_circuits(DG) ->
	Els = digraph:vertices(DG),
	graph_circuits_visit(Els, sets:new(), [], DG).
graph_circuits_visit([], _, Hoods, _) ->
	Hoods;
graph_circuits_visit([El | Rest], Visited, Hoods, DG) ->
	case sets:is_element(El, Visited) of
		true -> graph_circuits_visit(Rest, Visited, Hoods, DG);
		false ->
			Hood = graph_hood(DG, El),
			Visited1 = sets:union(Visited, Hood),
			graph_circuits_visit(Rest, Visited1, [sets:size(Hood) | Hoods], DG)
	end.

graph_hood(DG, El) ->
	Hood = sets:add_element(El, sets:new()),
	graph_hood(DG, graph_neighbors(DG, El, Hood), Hood).
graph_hood(_, [], Hood) ->
	Hood;
graph_hood(DG, [Node | Rest], Hood) ->
	Hood0 = sets:add_element(Node, Hood),
	Neigh = graph_neighbors(DG, Node, Hood0),
	Hood1 = graph_hood(DG, Neigh, Hood0),
	graph_hood(DG, Rest, Hood1).

graph_neighbors(DG, El, Hood) ->
	[
		graph_other(El, Edge)
		|| Edge <- digraph:out_edges(DG, El),
		sets:is_element(graph_other(El, Edge), Hood) =:= false
	].
graph_other(A, {A,B}) -> B;
graph_other(A, {B,A}) -> B.
