%%% @doc Bootstrap a sample by resampling in the data structure
%%% @end
-module(eministat_resample).

-include("eministat.hrl").

-export([resample/3, bootstrap_bca/4]).

%% @doc resample/3 is the main resampler of eministat
%% @end
resample(Estimators, Resamples, #dataset { n = N, points = Ps }) ->
    ResultSets = boot(Resamples, N, list_to_tuple(Ps)),
    estimate(Estimators, ResultSets).
    
boot(Resamples, N, Points) ->
    boot(Resamples, N, Points, []).
    
boot(0, _, _, Acc) -> Acc;
boot(K, N, Ps, Acc) ->
    Points = draw(N, N, Ps),
    boot(K-1, N, Ps, [eministat:ds_from_list(K, Points) | Acc]).
    
draw(0, _, _) -> [];
draw(K, N, Tuple) ->
    [element(rand:uniform(N), Tuple) | draw(K-1, N, Tuple)].

estimate([], _Results) -> [];
estimate([{Name, F} | Next], Results) ->
    Resamples = lists:sort([F(D) || D <- Results]),
    Rs = eministat:ds_from_list(Name, Resamples),
    [{Name, Rs} | estimate(Next, Results)].

bootstrap_bca(CLevel, Sample, Estimators, Resamples) when CLevel > 0 andalso CLevel < 1 ->
    [e(CLevel, Sample, Est, Resample) || {Est, Resample} <- lists:zip(Estimators, Resamples)].


e(CLevel, Sample, Est, #dataset { n = N, points = Ps }) ->
    PT = Est(Sample),
    
    Z1 = quantile(standard, (1 - CLevel) / 2),
    CumN = fun(X) -> round(N * cumulative(standard, X)) end,

    ProbN = count(fun(X) -> X < PT end, Ps),
    Bias = quantile(standard, ProbN / N),
    
    Jack = jackknife(Est, Sample),
    F = fun({S, C}, J) ->
        D = mean(Jack) - J,
        D2 = D * D,
        {S + D2, C + D2 * D}
    end,
    {SumSquares, SumCubes} = list:foldl(F, {0.0,0.0}, Jack),
    Accel = SumCubes / (6 * (math:pow(SumSquares, 1.5))),
    B1 = Bias + Z1,
    A1 = Bias + B1 / (1 - Accel * B1),
    Lo = max(0, CumN(A1)),
    
    B2 = Bias - Z1,
    A2 = Bias + B2 / (1 - Accel * B2),
    Hi = min(N - 1, CumN(A2)),
    
    #{ pt => PT, lo => Lo, hi => Hi, cl => CLevel }.

quantile(_, _) -> todo.

cumulative(_, _) -> todo.

mean(#dataset { n = N, sy = SY }) -> SY / N.

jackknife(mean, #dataset { n = N, points = Ps }) when N > 1 ->
    L = N-1,
    [(X + Y) / L || {X, Y} <- lists:zip(prefix_sum_l(Ps), prefix_sum_r(Ps))];
jackknife(_, _) -> todo.

prefix_sum_l(Points) -> scanl(fun erlang:'+'/2, 0.0, Points).
prefix_sum_r(Points) -> scanr(fun erlang:'+'/2, 0.0, Points).


%% -- STANDARD LIBRARY ROUTINES -----------------------------------------
%% Things which should have been in a standard library but isn't, one way or the other.

%% @doc count/2 counts how many times a predicate returns `true'
%% @end  
count(F, Ps) -> count(F, Ps, 0).

count(F, [P | Ps], K) ->
    case F(P) of
        true -> count(F, Ps, K+1);
        false -> count(F, Ps, K)
    end;
count(_F, [], K) -> K.

%% @doc scanl/3 is like foldl/3 but returns the accumulator for each iteration
%% @end
scanl(F, Q, Ls) ->
    case Ls of
       [] -> [Q];
       [X|Xs] -> [Q|scanl(F, F(X, Q), Xs)]
    end.

%% @doc scanr/3 is like foldr/3 but returns the accumulator for each iteration
%% @end
scanr(_F, Q0, []) -> [Q0];
scanr(F, Q0, [X|Xs]) ->
    Qs = [Q|_] = scanr(F, Q0, Xs),
    [F(X, Q) | Qs].
