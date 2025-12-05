-module(sol).
-compile(export_all).

-record(data, {fresh, ids}).

parse(Lines) ->
	parse_fresh(Lines, []).
parse_fresh(["" | Rest], Fresh) ->
	parse_ids(Rest, lists:reverse(Fresh), []);
parse_fresh([Range | Rest], Fresh) ->
	[Min, Max] = [list_to_integer(X) || X <- string:split(Range, "-")],
	parse_fresh(Rest, [{Min, Max} | Fresh]).
parse_ids([], Fresh, Ids) ->
	#data{fresh=Fresh, ids=lists:reverse(Ids)};
parse_ids([Id | Rest], Fresh, Ids) ->
	parse_ids(Rest, Fresh, [list_to_integer(Id) | Ids]).

sol1(#data{fresh=Fresh, ids=Ids}) ->
	support:count(fun(Id) ->
		lists:any(fun(Range) ->
			support:in_range(Id, Range)
		end, Fresh)
	end, Ids).

range_join({Aa, Az}, {Ba, Bz}) ->
	{min(Aa, Ba), max(Az, Bz)}.
range_overlaps({Aa, Az}, {Ba, Bz}) ->
	Aa =< Bz andalso Ba =< Az.

fresh_join(Fresh) -> fresh_join(Fresh, []).
fresh_join([], Joined) ->
	lists:reverse(Joined);
fresh_join([Range1 | Rest], Joined) ->
	{Overlap, Residue} = lists:partition(fun(Range2) ->
		range_overlaps(Range1, Range2)
	end, Rest),
	RangeJoined = lists:foldl(fun range_join/2, Range1, Overlap),
	fresh_join(Residue, [RangeJoined | Joined]).

sol2(#data{fresh=Fresh}) ->
	FreshSimple = support:fixpoint(fun fresh_join/1, Fresh),
	lists:sum([B - A + 1 || {A, B} <- FreshSimple]).
