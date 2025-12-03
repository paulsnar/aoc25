-module(sol).
-compile(export_all).

sol1(Lines) ->
	lists:sum([jmax(Line) || Line <- Lines]).

jmax(Line) ->
	Head = lists:sublist(Line, length(Line) - 1),
	Max = lists:max(lists:sublist(Line, length(Line) - 1)),
	Index = indexof(Max, Head),
	Max2 = lists:max(lists:sublist(Line, Index + 1, 100)),
	(Max - $0) * 10 + (Max2 - $0).

indexof(El, List) -> indexof(El, List, 1).
indexof(_, [], _) -> undefined;
indexof(El, [El | _], N) -> N;
indexof(El, [_ | Rest], N) -> indexof(El, Rest, N + 1).

