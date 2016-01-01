-module(eministat).

-include("eministat.hrl").

-export([
	s/3, s/4,
	x/3
]).

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

-define(EPSILON, 0.0000001). %% Epsilon value for floating point comparisons
-define(ROUNDS, 10000). %% Bootstrap resampling count
-define(MAX_DS, 8).

valid_ci(80.0) -> 1;
valid_ci(90.0) -> 2;
valid_ci(95.0) -> 3;
valid_ci(98.0) -> 4;
valid_ci(99.0) -> 5;
valid_ci(99.5) -> 6;
valid_ci(_) -> error(badarg).

symbol(K) when K >= 0 andalso K < ?MAX_DS -> [element(K, symbol())].

symbol() ->
    {$x, $+, $*, $%, $#, $@, $O}.

vitals_bootstrapped(#dataset { n = N } = Ds, Flag, CI) ->
    Boostrapped = eministat_resample:resample([mean, std_dev], ?ROUNDS, Ds),
    
    [{mean, #{ pt := Mean, lo := EMeanL, hi := EMeanH, mean := EMean } },
     {std_dev, #{ pt := StdDev, lo := EStdDevL, hi := EStdDevH, mean := EStdDev } }] =
        eministat_resample:bootstrap_bca(CI / 100, Ds, Boostrapped),

    Median = eministat_ds:median(Ds),
    Q1 = eministat_ds:percentile(0.25, Ds),
    Q3 = eministat_ds:percentile(0.75, Ds),
    io:format("Dataset: ~c N=~B CI=~g\n", [element(Flag, symbol()), N, CI]),

    io:format("Statistic     Value     [         Bias] (Bootstrapped LB‥UB)\n"),
    io:format("Min:      ~13g\n", [eministat_ds:min(Ds)]),
    io:format("1st Qu.   ~13g\n", [Q1]),
    io:format("Median:   ~13g\n", [Median]),
    io:format("3rd Qu.   ~13g\n", [Q3]),
    io:format("Max:      ~13g\n", [eministat_ds:max(Ds)]),
    io:format("Average:  ~13g [~13g] (~13g ‥ ~13g)\n",
        [Mean, EMean - Mean, EMeanL, EMeanH]),
    io:format("Std. Dev: ~13g [~13g] (~13g ‥ ~13g)\n", [StdDev, EStdDev - StdDev, EStdDevL, EStdDevH]),
    
    IQR = Q3 - Q1,
    OutliersB = length([x || P <- Ds#dataset.points, P < (Q1 - 1.5 * IQR)]),
    OutliersT = length([x || P <- Ds#dataset.points, P > (Q3 + 1.5 * IQR)]),
    io:format("~n"),

    io:format("Outliers: ~B/~B = ~B (μ=~g, σ=~g)\n", [OutliersB, OutliersT, OutliersT + OutliersB, EMean, EStdDev]),
    {OutVar, Severity} = eministat_analysis:outlier_variance(Mean, StdDev, N),
    io:format("\tOutlier variance: ~13g (~s)\n", [OutVar, format_outlier_variance(Severity)]),
    io:format("\n"),
    ok.

format_outlier_variance(unaffected) -> "not affected by outliers";
format_outlier_variance(slight) -> "slight";
format_outlier_variance(moderate) -> "moderate";
format_outlier_variance(severe) -> "severe, the data set is probably unusable".

plot_init(Width, Num) ->
    #plot { width = Width, height = 0, num_datasets = Num, min = 999.0e99, max = -999.0e99 }.

plot_adj(A, #plot { min = PMin, max = PMax, width = PWidth } = Plot) ->
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

plot_dim(Ds, Plot) ->
    lists:foldl(fun plot_adj/2, Plot,
        [eministat_ds:min(Ds),
         eministat_ds:max(Ds),
         eministat_ds:mean(Ds) - eministat_ds:std_dev(Ds),
         eministat_ds:mean(Ds) + eministat_ds:std_dev(Ds)]).

plot_height(#dataset { points = Ps }, #plot { dx = DX, x0 = X0 } = Plot) ->
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

plot_histo(#dataset { points = Ps }, Val, #plot { data = Data, dx = DX, x0 = X0 } = Plot) ->
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

plot_bar(#dataset {} = Ds, Pos, #plot { bars = Bars, dx = DX, x0 = X0 } = Plot) ->
    Bar =
      try eministat_ds:std_dev(Ds) of _Deviation ->
            X = trunc(((eministat_ds:mean(Ds) - eministat_ds:std_dev(Ds)) - X0) / DX),
            M = trunc(((eministat_ds:mean(Ds) + eministat_ds:std_dev(Ds)) - X0) / DX),
            Base = maps:from_list([{I, "_"} || I <- lists:seq(X+1, M-1)]),
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

plot_set(_Ds, _Val, #plot { span = S } = Plot) when S < ?EPSILON -> Plot;
plot_set(Ds, Val, Plot0) ->
    Plot1 = plot_height(Ds, Plot0),
    Plot2 = plot_histo(Ds, Val, Plot1),
    Plot3 = plot_bar(Ds, Val, Plot2),
    Plot3.

plot_dump(#plot { span = S }) when S < ?EPSILON ->
    io:format("[no plot, span is zero width]\n"),
    ok;
plot_dump(Plot) ->
    plot_dump_line(Plot),
    plot_dump_histo(Plot),
    plot_dump_bars(Plot),
    plot_dump_line(Plot),
    ok.

plot_dump_line(#plot { width = W }) ->
    io:format("+~s+\n", [binary:copy(<<"-">>, W)]),
    ok.

plot_dump_histo(#plot { width = W, height = H, data = Data }) ->
    PC = fun(I, J) ->
        case maps:get({I, J}, Data, undefined) of
            undefined -> " ";
            K -> symbol(K)
        end
    end,
    DumpLine = fun(I) -> [PC(I, J) || J <- lists:seq(0, W-1)] end,
    D = iolist_to_binary([ ["|", DumpLine(I), "|\n"] || I <- lists:seq(1, H)]),
    io:format("~s", [D]),
    ok.

plot_dump_bars(#plot{ num_datasets = N, bars = Bars, width = W }) ->
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

x(CI, #dataset{} = Ds0, #dataset{} = Ds1) ->
    x(CI, Ds0, [Ds1]);
x(CI, #dataset{} = Ds0, L) when is_list(L) ->
    case lists:all(fun(D) -> is_record(D, dataset) end, L) of
        true ->
            valid_ci(CI),
            h([Ds0 | L]),
            p(74, [Ds0 | L]),
            r(CI, Ds0, L);
        false ->
            error(badarg)
    end.

h(DSs) -> h(DSs, 1).

h([], _) -> ok;
h([#dataset { name = Name } | Next], Symb) ->
    io:format("~s ~s\n", [symbol(Symb), Name]),
    h(Next, Symb+1).

p(TermWidth, DSs) ->
    N = length(DSs),
    Init = plot_init(TermWidth, N),
    Dimensioned = lists:foldl(fun plot_dim/2, Init, DSs),
    {_, Plotted} = lists:foldl(fun(Ds, {Symb, Plot}) -> {Symb+1, plot_set(Ds, Symb, Plot)} end, {1, Dimensioned}, DSs),
    plot_dump(Plotted).

r(CI, Ds, DSs) ->
    vitals_bootstrapped(Ds, 1, CI),
    io:format("------\n\n"),

    r(CI, Ds, DSs, 1).

r(CI, Ds, [Rs | Next], I) ->
    vitals_bootstrapped(Rs, I+1, CI),
    eministat_analysis:relative(Rs, Ds, valid_ci(CI)),
    
    io:format("------\n\n"),
    r(CI, Ds, Next, I+1);
r(_, _, _, _) -> ok.

s(Name, F, Samples) -> s(Name, F, Samples, us).

s(Name, F, Samples, us) -> s(Name, F, Samples, fun(X) -> X end);
s(Name, F, Samples, ms) -> s(Name, F, Samples, fun(X) -> X div 1000 end);
s(Name, F, Samples, s) -> s(Name, F, Samples, fun(X) -> X div (1000*1000) end);
s(Name, F, Samples, G) when is_function(G, 1) ->
    eministat_ds:from_list(Name, s_run(F, G, Samples)).
    
s_run(F, G, K) ->
    s_warmup(F),
    s_bench(F, G, K).

s_warmup(F) ->
    erlang:send_after(3000, self(), warmup_over),
    s_warmup_loop(F).
    
s_warmup_loop(F) ->
    receive
        warmup_over -> ok
    after 0 ->
        F(),
        F(),
        F(),
        s_warmup_loop(F)
    end.

s_bench(_F, _G, 0) -> [];
s_bench(F, G, K) ->
    garbage_collect(),
    [G(element(1, timer:tc(F))) | s_bench(F,G, K-1)].
