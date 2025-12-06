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
transpose_isempty(Lists) ->
	lists:all(fun
		([]) -> true;
		(_) -> false
	end, Lists).

eval({add, Args}) ->
	lists:foldl(fun(X, Y) -> X + Y end, 0, Args);
eval({mul, Args}) ->
	lists:foldl(fun(X, Y) -> X * Y end, 1, Args).

sol(Ops) ->
	lists:sum([eval(Op) || Op <- Ops]).

parse2(Lines) ->
	Cols = transpose(Lines),
	parse2_start(Cols, []).
parse2_start([], Ops) ->
	lists:reverse(Ops);
parse2_start([NumAndOp | Rest], Ops) ->
	Op = case lists:last(NumAndOp) of
		$+ -> add;
		$* -> mul
	end,
	Arg = parse2_arg(NumAndOp),
	parse2_digits(Rest, [Arg], Op, Ops).
parse2_digits([], Args, Op, Ops) ->
	[{Op, Args} | Ops];
parse2_digits([Line | Rest], Args, Op, Ops) ->
	case string:trim(Line) of
		"" ->
			parse2_start(Rest, [{Op, Args} | Ops]);
		_ ->
			Arg = parse2_arg(Line),
			parse2_digits(Rest, [Arg | Args], Op, Ops)
	end.
parse2_arg(Line) ->
	Num = string:trim(withoutlast(Line)),
	list_to_integer(Num).

withoutlast(X) -> lists:sublist(X, 1, length(X) - 1).
