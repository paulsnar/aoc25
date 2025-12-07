-module(support).
-export([slurp/1, input/1, count/2, in_range/2, scan/3, scanmap/3, intpow/2,
	fixpoint/2, pipe/2]).

-spec slurp(iodata()) -> list(string()).
slurp(Filename) ->
    {ok, File} = file:open(Filename, [read, {encoding, utf8}]),
    Lines = slurp(File, []),
    file:close(File),
    Lines.

-spec slurp(any(), list(string())) -> list(string()).
slurp(File, Lines) ->
    case file:read_line(File) of
        eof -> lists:reverse(Lines);
        {ok, RawLine} ->
            Line = string:trim(RawLine, trailing, "\n"),
            slurp(File, [Line | Lines])
    end.

-spec input(string()) -> list(string()).
input(Text) ->
    string:split(Text, "\n", all).

-spec count(fun((X) -> boolean()), list(X)) -> integer().
count(Predicate, List) ->
    lists:foldl(fun (X, Sum) ->
        case Predicate(X) of
            true -> Sum + 1;
            false -> Sum
        end
    end, 0, List).

-spec in_range(integer(), {integer(), integer()}) -> boolean().
in_range(X, {Min, _}) when X < Min -> false;
in_range(X, {_, Max}) when X > Max -> false;
in_range(_, _) -> true.

-spec intpow(integer(), integer()) -> integer().
intpow(Base, Power) -> intpow(Base, Power, 1).
intpow(_, 0, Mul) -> Mul;
intpow(Base, Power, Mul) -> intpow(Base, Power - 1, Mul * Base).

-spec scan(fun((El, Acc) -> Acc), Acc, list(El)) -> list(Acc).
scan(Fun, Acc0, List) ->
	scan(Fun, Acc0, List, [Acc0]).
scan(_, _, [], Accs) -> lists:reverse(Accs);
scan(Fun, Acc1, [X | Rest], Accs) ->
	Acc2 = Fun(X, Acc1),
	scan(Fun, Acc2, Rest, [Acc2 | Accs]).

-spec scanmap(fun((El, Acc) -> Acc), Acc, list(El)) -> list({El, Acc}).
scanmap(Fun, Acc0, List) ->
	scan(fun (El, {_, Acc1}) ->
		Acc2 = Fun(El, Acc1),
		{El, Acc2}
	end, {nil, Acc0}, List).

-spec fixpoint(fun((Acc) -> Acc), Acc) -> Acc.
fixpoint(Fun, Acc1) ->
	Acc2 = Fun(Acc1),
	case Acc1 =:= Acc2 of
		true -> Acc1;
		false -> fixpoint(Fun, Acc2)
	end.

-spec pipe(list(fun((El) -> El)), El) -> El.
pipe([], El) -> El;
pipe([Fun | Rest], El) -> pipe(Rest, Fun(El)).
