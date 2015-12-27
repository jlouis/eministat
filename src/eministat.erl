-module(eministat).

-export([
	ds_from_list/2,
	s/3,
	x/3
]).

%% Two basic test data sets
-export([
	chameleon/0, iguana/0,
	ligustrum_sun/0, ligustrum_shade/0,
	reverse_1/0, reverse_2/0
]).

%% Data sets in eministat are these beasts
-record(dataset, {
	name :: string(),
	points :: [float()],
	sy :: float(),
	syy :: float(),
	n :: integer()
}).

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
-define(ROUNDS, 100000). %% Bootstrap resampling count
-define(NSTUDENT, 100). %% Number of elements in the students distribution lookup table
-define(MAX_DS, 8).

%% Constructing a dataset from a list
ds_from_list(Name, Ps) ->
    F = fun(P, {X, XX, K}) -> {X + P, XX + (P * P), K+1} end,
    {SY, SYY, N} = lists:foldl(F, {0.0, 0.0, 0}, Ps),
    case N < 3 of
        true ->
            error(dataset_too_small);
        false ->
            #dataset {
                name = Name,
                points = lists:sort(Ps),
                sy = SY,
                syy = SYY,
                n = N
            }
    end.

%% Constant tables, represented as tuples for O(1) lookup speeds
student_pct() -> {80.0, 90.0, 95.0, 98.0, 99.0, 99.5}.

valid_ci(80.0) -> 1;
valid_ci(90.0) -> 2;
valid_ci(95.0) -> 3;
valid_ci(98.0) -> 4;
valid_ci(99.0) -> 5;
valid_ci(99.5) -> 6;
valid_ci(_) -> error(badarg).

student_inf() ->
	{	1.282,	1.645,	1.960,	2.326,	2.576,	3.090  }. %%  inf

student() ->
  {
	{	3.078,	6.314,	12.706,	31.821,	63.657,	318.313  }, %%  1.
	{	1.886,	2.920,	4.303,	6.965,	9.925,	22.327  }, %%  2.
	{	1.638,	2.353,	3.182,	4.541,	5.841,	10.215  }, %%  3.
	{	1.533,	2.132,	2.776,	3.747,	4.604,	7.173  }, %%  4.
	{	1.476,	2.015,	2.571,	3.365,	4.032,	5.893  }, %%  5.
	{	1.440,	1.943,	2.447,	3.143,	3.707,	5.208  }, %%  6.
	{	1.415,	1.895,	2.365,	2.998,	3.499,	4.782  }, %%  7.
	{	1.397,	1.860,	2.306,	2.896,	3.355,	4.499  }, %%  8.
	{	1.383,	1.833,	2.262,	2.821,	3.250,	4.296  }, %%  9.
	{	1.372,	1.812,	2.228,	2.764,	3.169,	4.143  }, %%  10.
	{	1.363,	1.796,	2.201,	2.718,	3.106,	4.024  }, %%  11.
	{	1.356,	1.782,	2.179,	2.681,	3.055,	3.929  }, %%  12.
	{	1.350,	1.771,	2.160,	2.650,	3.012,	3.852  }, %%  13.
	{	1.345,	1.761,	2.145,	2.624,	2.977,	3.787  }, %%  14.
	{	1.341,	1.753,	2.131,	2.602,	2.947,	3.733  }, %%  15.
	{	1.337,	1.746,	2.120,	2.583,	2.921,	3.686  }, %%  16.
	{	1.333,	1.740,	2.110,	2.567,	2.898,	3.646  }, %%  17.
	{	1.330,	1.734,	2.101,	2.552,	2.878,	3.610  }, %%  18.
	{	1.328,	1.729,	2.093,	2.539,	2.861,	3.579  }, %%  19.
	{	1.325,	1.725,	2.086,	2.528,	2.845,	3.552  }, %%  20.
	{	1.323,	1.721,	2.080,	2.518,	2.831,	3.527  }, %%  21.
	{	1.321,	1.717,	2.074,	2.508,	2.819,	3.505  }, %%  22.
	{	1.319,	1.714,	2.069,	2.500,	2.807,	3.485  }, %%  23.
	{	1.318,	1.711,	2.064,	2.492,	2.797,	3.467  }, %%  24.
	{	1.316,	1.708,	2.060,	2.485,	2.787,	3.450  }, %%  25.
	{	1.315,	1.706,	2.056,	2.479,	2.779,	3.435  }, %%  26.
	{	1.314,	1.703,	2.052,	2.473,	2.771,	3.421  }, %%  27.
	{	1.313,	1.701,	2.048,	2.467,	2.763,	3.408  }, %%  28.
	{	1.311,	1.699,	2.045,	2.462,	2.756,	3.396  }, %%  29.
	{	1.310,	1.697,	2.042,	2.457,	2.750,	3.385  }, %%  30.
	{	1.309,	1.696,	2.040,	2.453,	2.744,	3.375  }, %%  31.
	{	1.309,	1.694,	2.037,	2.449,	2.738,	3.365  }, %%  32.
	{	1.308,	1.692,	2.035,	2.445,	2.733,	3.356  }, %%  33.
	{	1.307,	1.691,	2.032,	2.441,	2.728,	3.348  }, %%  34.
	{	1.306,	1.690,	2.030,	2.438,	2.724,	3.340  }, %%  35.
	{	1.306,	1.688,	2.028,	2.434,	2.719,	3.333  }, %%  36.
	{	1.305,	1.687,	2.026,	2.431,	2.715,	3.326  }, %%  37.
	{	1.304,	1.686,	2.024,	2.429,	2.712,	3.319  }, %%  38.
	{	1.304,	1.685,	2.023,	2.426,	2.708,	3.313  }, %%  39.
	{	1.303,	1.684,	2.021,	2.423,	2.704,	3.307  }, %%  40.
	{	1.303,	1.683,	2.020,	2.421,	2.701,	3.301  }, %%  41.
	{	1.302,	1.682,	2.018,	2.418,	2.698,	3.296  }, %%  42.
	{	1.302,	1.681,	2.017,	2.416,	2.695,	3.291  }, %%  43.
	{	1.301,	1.680,	2.015,	2.414,	2.692,	3.286  }, %%  44.
	{	1.301,	1.679,	2.014,	2.412,	2.690,	3.281  }, %%  45.
	{	1.300,	1.679,	2.013,	2.410,	2.687,	3.277  }, %%  46.
	{	1.300,	1.678,	2.012,	2.408,	2.685,	3.273  }, %%  47.
	{	1.299,	1.677,	2.011,	2.407,	2.682,	3.269  }, %%  48.
	{	1.299,	1.677,	2.010,	2.405,	2.680,	3.265  }, %%  49.
	{	1.299,	1.676,	2.009,	2.403,	2.678,	3.261  }, %%  50.
	{	1.298,	1.675,	2.008,	2.402,	2.676,	3.258  }, %%  51.
	{	1.298,	1.675,	2.007,	2.400,	2.674,	3.255  }, %%  52.
	{	1.298,	1.674,	2.006,	2.399,	2.672,	3.251  }, %%  53.
	{	1.297,	1.674,	2.005,	2.397,	2.670,	3.248  }, %%  54.
	{	1.297,	1.673,	2.004,	2.396,	2.668,	3.245  }, %%  55.
	{	1.297,	1.673,	2.003,	2.395,	2.667,	3.242  }, %%  56.
	{	1.297,	1.672,	2.002,	2.394,	2.665,	3.239  }, %%  57.
	{	1.296,	1.672,	2.002,	2.392,	2.663,	3.237  }, %%  58.
	{	1.296,	1.671,	2.001,	2.391,	2.662,	3.234  }, %%  59.
	{	1.296,	1.671,	2.000,	2.390,	2.660,	3.232  }, %%  60.
	{	1.296,	1.670,	2.000,	2.389,	2.659,	3.229  }, %%  61.
	{	1.295,	1.670,	1.999,	2.388,	2.657,	3.227  }, %%  62.
	{	1.295,	1.669,	1.998,	2.387,	2.656,	3.225  }, %%  63.
	{	1.295,	1.669,	1.998,	2.386,	2.655,	3.223  }, %%  64.
	{	1.295,	1.669,	1.997,	2.385,	2.654,	3.220  }, %%  65.
	{	1.295,	1.668,	1.997,	2.384,	2.652,	3.218  }, %%  66.
	{	1.294,	1.668,	1.996,	2.383,	2.651,	3.216  }, %%  67.
	{	1.294,	1.668,	1.995,	2.382,	2.650,	3.214  }, %%  68.
	{	1.294,	1.667,	1.995,	2.382,	2.649,	3.213  }, %%  69.
	{	1.294,	1.667,	1.994,	2.381,	2.648,	3.211  }, %%  70.
	{	1.294,	1.667,	1.994,	2.380,	2.647,	3.209  }, %%  71.
	{	1.293,	1.666,	1.993,	2.379,	2.646,	3.207  }, %%  72.
	{	1.293,	1.666,	1.993,	2.379,	2.645,	3.206  }, %%  73.
	{	1.293,	1.666,	1.993,	2.378,	2.644,	3.204  }, %%  74.
	{	1.293,	1.665,	1.992,	2.377,	2.643,	3.202  }, %%  75.
	{	1.293,	1.665,	1.992,	2.376,	2.642,	3.201  }, %%  76.
	{	1.293,	1.665,	1.991,	2.376,	2.641,	3.199  }, %%  77.
	{	1.292,	1.665,	1.991,	2.375,	2.640,	3.198  }, %%  78.
	{	1.292,	1.664,	1.990,	2.374,	2.640,	3.197  }, %%  79.
	{	1.292,	1.664,	1.990,	2.374,	2.639,	3.195  }, %%  80.
	{	1.292,	1.664,	1.990,	2.373,	2.638,	3.194  }, %%  81.
	{	1.292,	1.664,	1.989,	2.373,	2.637,	3.193  }, %%  82.
	{	1.292,	1.663,	1.989,	2.372,	2.636,	3.191  }, %%  83.
	{	1.292,	1.663,	1.989,	2.372,	2.636,	3.190  }, %%  84.
	{	1.292,	1.663,	1.988,	2.371,	2.635,	3.189  }, %%  85.
	{	1.291,	1.663,	1.988,	2.370,	2.634,	3.188  }, %%  86.
	{	1.291,	1.663,	1.988,	2.370,	2.634,	3.187  }, %%  87.
	{	1.291,	1.662,	1.987,	2.369,	2.633,	3.185  }, %%  88.
	{	1.291,	1.662,	1.987,	2.369,	2.632,	3.184  }, %%  89.
	{	1.291,	1.662,	1.987,	2.368,	2.632,	3.183  }, %%  90.
	{	1.291,	1.662,	1.986,	2.368,	2.631,	3.182  }, %%  91.
	{	1.291,	1.662,	1.986,	2.368,	2.630,	3.181  }, %%  92.
	{	1.291,	1.661,	1.986,	2.367,	2.630,	3.180  }, %%  93.
	{	1.291,	1.661,	1.986,	2.367,	2.629,	3.179  }, %%  94.
	{	1.291,	1.661,	1.985,	2.366,	2.629,	3.178  }, %%  95.
	{	1.290,	1.661,	1.985,	2.366,	2.628,	3.177  }, %%  96.
	{	1.290,	1.661,	1.985,	2.365,	2.627,	3.176  }, %%  97.
	{	1.290,	1.661,	1.984,	2.365,	2.627,	3.175  }, %%  98.
	{	1.290,	1.660,	1.984,	2.365,	2.626,	3.175  }, %%  99.
	{	1.290,	1.660,	1.984,	2.364,	2.626,	3.174  } %%  100.
  }.

student_lookup(I) when I > ?NSTUDENT -> student_inf();
student_lookup(I) -> element(I, student()).

symbol(K) when K >= 0 andalso K < ?MAX_DS -> [element(K, symbol())].

symbol() ->
    {$x, $+, $*, $%, $#, $@, $O}.

%% Simple mathematical representations over data sets
min(#dataset { points = Ps }) -> float(hd(Ps)).

max(#dataset { points = Ps }) -> float(lists:last(Ps)).

average(#dataset { n = N, sy = SumY }) -> float(SumY / N).

median(#dataset { n = N, points = Ps }) ->
    float(lists:nth(round(N / 2), Ps)).

variance(#dataset { n = N, sy = SY, syy = SYY }) ->
    (SYY - SY * SY / N) / (N - 1.0).

std_dev(Ds) -> math:sqrt(variance(Ds)).

vitals_bootstrapped(Ds, Flag, CI) ->
    BDs = bootstrap(?ROUNDS, Ds),
    io:format("Dataset: ~c N=~B CI=~g\n", [element(Flag, symbol()), Ds#dataset.n, CI]),
    io:format("Statistic     Value     [     Bias] (SE)\n"),
    io:format("Min:      ~13.8g\n", [min(Ds)]),
    {AvgMedian, SDMedian} = boot_result(fun median/1, BDs),
    Median = median(Ds),
    io:format("Median:   ~13.8g [~9.4g] (± ~g)\n", [Median, AvgMedian - Median, SDMedian]),
    io:format("Max:      ~13.8g\n", [max(Ds)]),
    {AvgAverage, SDAverage} = boot_result(fun average/1, BDs),
    Average = average(Ds),
    io:format("Average:  ~13.8g [~9.4g] (± ~g)\n", [Average, AvgAverage - Average, SDAverage]),
    {AvgStdDev, SDStdDev} = boot_result(fun std_dev/1, BDs),
    StdDev = std_dev(Ds),
    io:format("Std. Dev: ~13.8g [~9.4g] (± ~g)\n", [StdDev, AvgStdDev - StdDev, SDStdDev]),
    io:format("\n"),
    ok.

bootstrap(Rounds, #dataset { n = N, points = Ps }) ->
    boot(Rounds, N, list_to_tuple(Ps), []).
    
boot(0, _, _, Acc) -> Acc;
boot(K, N, Ps, Acc) ->
    Points = draw(N, N, Ps),
    boot(K-1, N, Ps, [ds_from_list(K, Points) | Acc]).

draw(0, _, _) -> [];
draw(K, N, Tuple) ->
    [element(rand:uniform(N), Tuple) | draw(K-1, N, Tuple)].

boot_result(Fun, Ds) ->
    Resamples = lists:sort([Fun(D) || D <- Ds]),
    Rs = ds_from_list(resampled, Resamples),
    {average(Rs), std_dev(Rs)}.

relative(#dataset { n = DsN } = Ds, #dataset { n = RsN } = Rs, ConfIdx) ->
    I = DsN + RsN - 2,
    T = element(ConfIdx, student_lookup(I)),

    Spool1 = (DsN - 1) * variance(Ds) + (RsN - 1) * variance(Rs),
    Spool = math:sqrt(Spool1 / I),

    S = Spool * math:sqrt(1.0 / DsN + 1.0 / RsN),
    D = average(Ds) - average(Rs),
    E = T * S,

    case abs(D) > E of
        false ->
            io:format("No difference proven at ~.1f% confidence\n",
                [element(ConfIdx, student_pct())]);
        true ->
            io:format("Difference at ~.1f% confidence\n", [element(ConfIdx, student_pct())]),
            io:format("	~g +/- ~g\n", [D, E]),
            io:format("	~g% +/- ~g%\n", [D * 100 / average(Rs), E * 100 / average(Rs)]),
            io:format("	(Student's t, pooled s = ~g)\n", [Spool])
    end.

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
        [min(Ds),
         max(Ds),
         average(Ds) - std_dev(Ds),
         average(Ds) + std_dev(Ds)]).

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
      try std_dev(Ds) of _Deviation ->
            X = trunc(((average(Ds) - std_dev(Ds)) - X0) / DX),
            M = trunc(((average(Ds) + std_dev(Ds)) - X0) / DX),
            Base = maps:from_list([{I, "_"} || I <- lists:seq(X+1, M-1)]),
            Base#{ M => "|", X => "|" }
      catch
          error:badarith ->
              #{}
    end,
    Median = trunc((median(Ds) - X0) / DX),
    Average = trunc((average(Ds) - X0) / DX),
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
    r(CI, Ds, DSs, 1).

r(CI, Ds, [Rs | Next], I) ->
    vitals_bootstrapped(Rs, I+1, CI),
    relative(Rs, Ds, valid_ci(CI)),

    r(CI, Ds, Next, I+1);
r(_, _, _, _) -> ok.

s(Name, F, Samples) ->
    ds_from_list(Name, s(F, Samples)).
    
s(_F, 0) -> [];
s(F, K) ->
    [element(1, timer:tc(F)) | s(F,K-1)].
    

chameleon() ->
    ds_from_list("chameleon", [150, 400, 720, 500, 930]).

iguana() ->
    ds_from_list("iguana", [50, 200, 150, 400, 750, 400, 150]).

ligustrum_sun() ->
    ds_from_list("sun", [150, 100, 210, 300, 200, 210, 300]).

ligustrum_shade() ->
    ds_from_list("shade", [120, 125, 160, 130, 200, 170, 200]).

reverse_1() ->
    L = lists:seq(1, 100000),
    s("lists:reverse/1", fun() -> lists:reverse(L) end, 50).

reverse_2() ->
    L = lists:seq(1, 100000),
    s("tail_reverse/1", fun() -> tail_reverse(L) end, 50).

tail_reverse(L) -> tail_reverse(L, []).

tail_reverse([], Acc) -> Acc;
tail_reverse([X | Xs], Acc) -> tail_reverse(Xs, [X | Acc]).
