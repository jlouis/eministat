-module(eministat_report).

-export([symbol/1, vitals/2]).

-define(MAX_DS, 8).

symbol(K) when K >= 0 andalso K < ?MAX_DS -> [element(K, symbol())].

symbol() ->
    {$x, $+, $*, $%, $#, $@, $O}.

vitals(#{ max := Max, min := Min, n := N, ci := CI,
    outliers := {OutliersB, OutliersT},
    percentiles := #{ 25 := Q1, 50 := Median, 75 := Q3 },
    bootstrapped := #{
        mean := #{ pt := Mean, lo := EMeanL, hi := EMeanH, mean := EMean },
        std_dev := #{ pt := StdDev, lo := EStdDevL, hi := EStdDevH, mean := EStdDev }
  }}, Flag) ->
    io:format("Dataset: ~c N=~B CI=~g\n", [element(Flag, symbol()), N, CI]),

    io:format("Statistic     Value     [         Bias] (Bootstrapped LB‥UB)\n"),
    io:format("Min:      ~13g\n", [Min]),
    io:format("1st Qu.   ~13g\n", [Q1]),
    io:format("Median:   ~13g\n", [Median]),
    io:format("3rd Qu.   ~13g\n", [Q3]),
    io:format("Max:      ~13g\n", [Max]),
    io:format("Average:  ~13g [~13g] (~13g ‥ ~13g)\n",
        [Mean, EMean - Mean, EMeanL, EMeanH]),
    io:format("Std. Dev: ~13g [~13g] (~13g ‥ ~13g)\n", [StdDev, EStdDev - StdDev, EStdDevL, EStdDevH]),
    
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

