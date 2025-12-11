-module(sol).
-compile(export_all).

parse(Lines) -> parse(Lines, #{}).
parse([], Graph) ->
	Graph;
parse([Line | Rest], Graph) ->
	[Node, OutputsRaw] = string:split(Line, ": "),
	Outputs = string:split(OutputsRaw, " ", all),
	parse(Rest, Graph#{Node => Outputs}).

visit("out", History, _, _) ->
	[lists:reverse(["out" | History])];
visit(Node, History, Visited, Graph) ->
	case sets:is_element(Node, Visited) of
		true -> [];
		false ->
			Hist1 = [Node | History],
			Vis1 = sets:add_element(Node, Visited),
			Neighs = maps:get(Node, Graph, []),
			visit_children(Neighs, Hist1, Vis1, Graph)
	end.
visit_children([], _, _, _) ->
	[];
visit_children([Node | Rest], Hist1, Vis1, Graph) ->
	visit(Node, Hist1, Vis1, Graph) ++
	visit_children(Rest, Hist1, Vis1, Graph).

sol1(Graph) ->
	Paths = visit("you", [], sets:new(), Graph),
	length(Paths).
