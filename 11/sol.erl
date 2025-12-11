-module(sol).
-compile(export_all).

parse(Lines) -> parse(Lines, #{}).
parse([], Graph) ->
	Graph;
parse([Line | Rest], Graph) ->
	[Node, OutputsRaw] = string:split(Line, ": "),
	Outputs = string:split(OutputsRaw, " ", all),
	parse(Rest, Graph#{Node => Outputs}).

sol1(Graph) ->
	flood_start("you", "out", Graph).

flood_start(Start, Dest, Graph) ->
	{ok, Deg} = flood([Start], #{}, Dest, Graph),
	maps:get(Start, Deg).

flood([Dest | Q], Deg, Dest, Graph) ->
	Deg1 = Deg#{Dest => 1},
	flood(Q, Deg1, Dest, Graph);
flood([Node | Q], Deg, Dest, Graph) ->
	case maps:is_key(Node, Deg) of
		true -> flood(Q, Deg, Dest, Graph);
		false ->
			case flood_pending_neigh(Node, Deg, Graph) of
				[] ->
					Deg1 = flood_update(Node, Deg, Graph),
					flood(Q, Deg1, Dest, Graph);
				Neigh ->
					Q1 = Neigh ++ [Node | Q],
					flood(Q1, Deg, Dest, Graph)
			end
	end;
flood([], Deg, _, _) ->
	{ok, Deg}.

flood_pending_neigh(Node, Deg, Graph) ->
	Neighs = maps:get(Node, Graph, []),
	[Neigh || Neigh <- Neighs, not maps:is_key(Neigh, Deg)].
flood_update(Node, Deg, Graph) ->
	case maps:get(Node, Graph, nil) of
		nil -> Deg#{Node => 0};
		Neighs ->
			NodeDeg = lists:sum([maps:get(N, Deg) || N <- Neighs]),
			Deg#{Node => NodeDeg}
	end.

sol2(Graph) ->
	SvrFft = flood_start("svr", "fft", Graph),
	FftDac = flood_start("fft", "dac", Graph),
	DacOut = flood_start("dac", "out", Graph),

	SvrDac = flood_start("svr", "dac", Graph),
	DacFft = flood_start("dac", "fft", Graph),
	FftOut = flood_start("fft", "out", Graph),

	{SvrFft * FftDac * DacOut, SvrDac * DacFft * FftOut}.
