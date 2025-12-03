-module(sol).
-compile(export_all).

sol1(Lines) ->
	lists:sum([jmax(Line, 2) || Line <- Lines]).

sol2(Lines) ->
	lists:sum([jmax(Line, 12) || Line <- Lines]).

jmax(Line, Length) ->
	jmax(lists:reverse(Line), Length, []).

jmax(_, 0, Captured) ->
	list_to_integer(lists:reverse(Captured));
jmax(Line, Length, Captured) ->
	{Protected, Search} = lists:split(Length - 1, Line),
	{Top, NextSearch} = jmax_search(Search, []),
	jmax(Protected ++ NextSearch, Length - 1, [Top | Captured]).

jmax_search([El], Discard) ->
	{El, lists:reverse(Discard)};
jmax_search([El | Rest], Discard) ->
	case has_greater(El, Rest) of
		true -> jmax_search(Rest, [El | Discard]);
		false -> {El, lists:reverse(Discard)}
	end.

has_greater(_, []) -> false;
has_greater(X, [Y | _]) when Y >= X -> true;
has_greater(X, [_ | Rest]) -> has_greater(X, Rest).
