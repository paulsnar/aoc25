-module(sol).
-compile(export_all).

parse(Lines) ->
	parse_line(Lines, []).
parse_line([OpLine], ArgLines) ->
	Lexemes = string:lexemes(OpLine, " "),
	Ops = [case X of
		"+" -> add;
		"*" -> mul
	end || X <- Lexemes],
	parse_check_lengths(length(Ops), ArgLines),
	lists:zip(Ops, transpose(lists:reverse(ArgLines)));
parse_line([NumLine | Rest], ArgLines) ->
	Lexemes = string:lexemes(NumLine, " "),
	ArgLine = [list_to_integer(X) || X <- Lexemes],
	parse_line(Rest, [ArgLine | ArgLines]).
parse_check_lengths(_, []) -> ok;
parse_check_lengths(Length, [Line | Rest]) ->
	Length = length(Line),
	parse_check_lengths(Length, Rest).

transpose(Lines) -> transpose(Lines, []).
transpose(Lines, Transposed) ->
	case transpose_isempty(Lines) of
		true -> lists:reverse(Transposed);
		false ->
			{Heads, Tails} = transpose_hdtl(Lines, [], []),
			transpose(Tails, [Heads | Transposed])
	end.
transpose_hdtl([], Heads, Tails) ->
	{lists:reverse(Heads), lists:reverse(Tails)};
transpose_hdtl([List | Rest], Heads, Tails) ->
	[Head | Tail] = List,
	transpose_hdtl(Rest, [Head | Heads], [Tail | Tails]).
transpose_isempty([]) -> true;
transpose_isempty([[] | Rest]) -> transpose_isempty(Rest);
transpose_isempty([[_ | _] | _]) -> false.

eval({add, Args}) ->
	lists:foldl(fun(X, Y) -> X + Y end, 0, Args);
eval({mul, Args}) ->
	lists:foldl(fun(X, Y) -> X * Y end, 1, Args).

sol1(Ops) ->
	lists:sum([eval(Op) || Op <- Ops]).

