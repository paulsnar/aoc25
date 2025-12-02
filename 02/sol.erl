-module(sol).
-compile(export_all).

parse(Line) ->
	Ranges = string:split(Line, ",", all),
	RangesEl = lists:map(fun (Range) ->
		[Min, Max] = [list_to_integer(X) || X <- string:split(Range, "-", all)],
		{Min, Max}
	end, Ranges),
	lists:sort(RangesEl).

candidates(Length) ->
	case Length rem 2 of
		0 ->
			L2 = Length div 2,
			[imul(X, L2) || X <- lists:seq(imin(L2), imax(L2))];
		1 -> []
	end.
candidates_bucket(Length, Bucket, CandidateGen) ->
	[{Min, _} | _] = Bucket,
	{_, Max} = lists:last(Bucket),
	lists:filter(fun(Candidate) ->
		support:in_range(Candidate, {Min, Max})
	end, CandidateGen(Length)).

imul(X, N) -> X * (support:intpow(10, N) + 1).
imin(N) -> support:intpow(10, N - 1).
imax(N) -> support:intpow(10, N) - 1.

update_range(Ranges, Bucket, Range) ->
	case maps:get(Bucket, Ranges, undefined) of
		undefined -> [Range];
		List -> [Range | List]
	end.
reorganize(Ranges) ->
	LengthGrouped = lists:foldl(fun({Min, Max}, Acc) ->
		LenMin = length(integer_to_list(Min)),
		LenMax = length(integer_to_list(Max)),
		case LenMin =:= LenMax of
			true -> Acc#{LenMin => update_range(Acc, LenMin, {Min, Max})};
			false ->
				% range goes off end of LenMin and crosses into LenMax
				Acc#{
					LenMin => update_range(Acc, LenMin, {Min, imax(LenMin)}),
					LenMax => update_range(Acc, LenMax, {imin(LenMax), Max})
				}
		end
	end, maps:new(), Ranges),
	reorganize_finalize([], maps:iterator(LengthGrouped, ordered)).
reorganize_finalize(Accum1, Iter1) ->
	case maps:next(Iter1) of
		{Length, Ranges, Iter2} ->
			Accum2 = [{Length, lists:sort(Ranges)} | Accum1],
			reorganize_finalize(Accum2, Iter2);
		none -> lists:reverse(Accum1)
	end.

in_any_range(_, []) -> false;
in_any_range(Val, [{Min, Max} | _]) when Val >= Min, Val =< Max -> true;
in_any_range(Val, [_ | Rest]) -> in_any_range(Val, Rest).

eval(Length, Bucket, CandidateGen) ->
	[
		Val
		|| Val <- candidates_bucket(Length, Bucket, CandidateGen),
			in_any_range(Val, Bucket)
	].

sol1(Ranges) ->
	R1 = reorganize(Ranges),
	lists:foldl(fun({Length, Bucket}, Acc) ->
		Acc + lists:sum(eval(Length, Bucket, fun candidates/1))
	end, 0, R1).

% Build a repeated integer consisting of X repeated N times.
% X is evaluated in base 10 without leading zeroes.
irep(X, N) -> irep(X, N, 0, support:intpow(10, 1 + floor(math:log10(X)))).
irep(_, 0, Acc, _) -> Acc;
irep(X, N, Acc, Base) -> irep(X, N - 1, Acc * Base + X, Base).

candidates2(Length) ->
	All = lists:flatten([
		candidates2_part(Length, LenPart)
		|| LenPart <- lists:seq(1, Length div 2)
	]),
	% some patterns might overlap, such as "2"x6 and "22"x3 and "222"x2
	lists:uniq(All).
candidates2_part(Length, LenPart) ->
	Repeat = Length div LenPart,
	[irep(X, Repeat) || X <- lists:seq(imin(LenPart), imax(LenPart))].

sol2(Ranges) ->
	R1 = reorganize(Ranges),
	lists:foldl(fun({Length, Bucket}, Acc) ->
		Acc + lists:sum(eval(Length, Bucket, fun candidates2/1))
	end, 0, R1).
