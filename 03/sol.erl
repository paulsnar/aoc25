-module(sol).
-compile(export_all).

sol1(Lines) ->
	lists:sum([jmax(Line, 2) || Line <- Lines]).

sol2(Lines) ->
	lists:sum([jmax(Line, 12) || Line <- Lines]).

jmax(Line, Length) ->
	jmax(lists:reverse(Line), Length, []).

% The core insight stems from the observation that for each digit there's
% a local optimum – pick the leftmost digit that's greater or equal to
% any rightward digits. Therefore we only need O(N) iterations, each of which
% has O(N^2) subiterations (could be optimized by precomputing lists:max
% at each iteration, making it O(N) as well).
%
% Here we process it in reverse because that's more amenable to Erlang's
% semantics (iterating lists "forwards" is easy, iterating them backwards
% is essentially impossible) hence the littering of lists:reverse.

jmax(_, 0, Captured) ->
	list_to_integer(lists:reverse(Captured));
jmax(Line, Length, Captured) ->
	% Do not search within the right N-1 digits - even if a maximum can
	% be found there, we won't be able to use it when actually constructing
	% the number since there will be insufficient digits to the right of it.
	{Protected, Search} = lists:split(Length - 1, Line),
	{Top, NextSearch} = jmax_search(Search, []),
	jmax(Protected ++ NextSearch, Length - 1, [Top | Captured]).

% During search of a top digit, accumulate all the previous digits in the
% discard list. Next search will be bounded to the discard list's contents
% (digits that were rightwards of the max digit) since we can't use any
% digits to the left of the local optimum.
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
