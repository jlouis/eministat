-module(eministat_plot).
-include("eministat.hrl").

-export([p/1, p/2]).

%% Record for the plotting infrastructure
-record(plot, {
	width :: non_neg_integer(),
	height :: non_neg_integer(),
	data = #{},
	bars = #{},
	num_datasets :: non_neg_integer(),
	min :: float(),
	max :: float(),
	span :: float(),
	dx :: float(),
	x0 :: float()
}).

-define(DEFAULT_TERM_WIDTH, 74).
-define(EPSILON, 0.0000001). %% Epsilon value for floating point comparisons

%% -- API -----------------------------------------------------------------------------

p(DSs) -> p(?DEFAULT_TERM_WIDTH, DSs).

p(TermWidth, DSs) ->
    N = length(DSs),
    Init = mk(TermWidth, N),
    Dimensioned = lists:foldl(fun dim/2, Init, DSs),
    {_, Plotted} = lists:foldl(fun(Ds, {Symb, Plot}) -> {Symb+1, set(Ds, Symb, Plot)} end, {1, Dimensioned}, DSs),
    dump(Plotted).

%% -- INTERNALS ---------------------------------------------------------------------------

set(_Ds, _Val, #plot { span = S } = Plot) when S < ?EPSILON -> Plot;
set(Ds, Val, Plot0) ->
    Plot1 = height(Ds, Plot0),
    Plot2 = histo(Ds, Val, Plot1),
    Plot3 = bar(Ds, Val, Plot2),
    Plot3.

%% init/2 sets up a new plot structure
mk(Width, Num) ->
    #plot { width = Width, height = 0, num_datasets = Num, min = 999.0e99, max = -999.0e99 }.

adj(A, #plot { min = PMin, max = PMax, width = PWidth } = Plot) ->
    Min = min(A, PMin),
    Max = max(A, PMax),
    Span = Max - Min,
    DX = Span / (PWidth - 1.0),
    X0 = PMin - 0.5*DX,
    Plot#plot {
        min = Min,
        max = Max,
        span = Span,
        dx = DX,
        x0 = X0
    }.

dim(Ds, Plot) ->
    Mean = eministat_ds:mean(Ds),
    Dev = eministat_ds:std_dev(Ds),
    lists:foldl(fun adj/2, Plot,
        [eministat_ds:min(Ds),
         eministat_ds:max(Ds),
         Mean - Dev,
         Mean + Dev]).

height(#dataset { points = Ps }, #plot { dx = DX, x0 = X0 } = Plot) ->
    F = fun(P, {M, I, J}) ->
        X = trunc((P - X0) / DX),
        case X == I of
            true ->
                {max(J+1, M), I, J+1};
            false ->
                {M, X, 1}
        end
    end,
    {FM, _, _} = lists:foldl(F, {1, -1, 0}, Ps),
    Plot#plot { height = FM }.

histo(#dataset { points = Ps }, Val, #plot { data = Data, dx = DX, x0 = X0 } = Plot) ->
    F = fun(P, {PlotMap, I, J}) ->
        X = trunc((P - X0) / DX),
        {II, JJ} = case X == I of
            true -> {I, J+1};
            false -> {X, 1}
        end,
        {push(PlotMap, II, JJ, Val), II, JJ}
    end,
    {PlotMap, _, _} = lists:foldl(F, {Data, -1, 1}, Ps),
    Plot#plot { data = PlotMap }.

bar(#dataset {} = Ds, Pos, #plot { bars = Bars, dx = DX, x0 = X0 } = Plot) ->
    Bar =
      try eministat_ds:std_dev(Ds) of Dev ->
              X = trunc(((eministat_ds:mean(Ds) - Dev) - X0) / DX),
              M = trunc(((eministat_ds:mean(Ds) + Dev) - X0) / DX),
              Base = case {X+1, M-1} of
                         {Z, Z} -> #{ Z => "_" };
                         {Lo, Hi} when Lo < Hi ->
                             maps:from_list([{I, "_"} || I <- lists:seq(Lo, Hi)]);
                         {_, _} -> #{}
                     end,
              Base#{ M => "|", X => "|" }
      catch
          error:badarith ->
              #{}
    end,
    Median = trunc((eministat_ds:median(Ds) - X0) / DX),
    Average = trunc((eministat_ds:mean(Ds) - X0) / DX),
    FinalBar = Bar#{ Median => "M", Average => "A" },
    Plot#plot { bars = Bars# { Pos => FinalBar } }.

push(M, I, J, Val) ->
    case maps:get({J, I}, M, undefined) of
        undefined -> M#{ {J, I} => Val };
        V -> M#{ {J, I} := V bxor Val }
    end.


dump(#plot { span = S }) when S < ?EPSILON ->
    {error, zero_span};
dump(Plot) ->
    dump_line(Plot),
    dump_histo(Plot),
    dump_bars(Plot),
    dump_line(Plot),
    ok.

dump_line(#plot { width = W }) ->
    io:format("+~s+\n", [binary:copy(<<"-">>, W)]),
    ok.

dump_histo(#plot { width = W, height = H, data = Data }) ->
    PC = fun(I, J) ->
        case maps:get({I, J}, Data, undefined) of
            undefined -> " ";
            K -> eministat_report:symbol(K)
        end
    end,
    DumpLine = fun(I) -> [PC(I, J) || J <- lists:seq(0, W-1)] end,
    D = iolist_to_binary([ ["|", DumpLine(I), "|\n"] || I <- lists:seq(1, H)]),
    io:format("~s", [D]),
    ok.

dump_bars(#plot{ num_datasets = N, bars = Bars, width = W }) ->
    DumpBar = fun(B) ->
        case maps:get(B, Bars, undefined) of
            undefined -> ok;
            Bar ->
                Out = [maps:get(J, Bar, " ") || J <- lists:seq(0, W-1)],
                io:format("|~s|\n", [iolist_to_binary(Out)])
        end
    end,
    [DumpBar(K) || K <- lists:seq(1, N)],
    ok.
