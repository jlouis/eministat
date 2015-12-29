%%% @doc Bootstrap a sample by resampling in the data structure
%%% @end
-module(eministat_resample).

-include("eministat.hrl").

-export([bootstrap/3, bootstrap_bca/4]).
-compile(export_all).

%% @doc resample/3 is the main resampler of eministat
%% @end
bootstrap(Estimators, Resamples, #dataset { n = N, points = Ps }) ->
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
estimate([Name | Next], Results) ->
    Resamples = lists:sort([estimator(Name, D) || D <- Results]),
    Rs = eministat:ds_from_list(Name, Resamples),
    [{Name, Rs} | estimate(Next, Results)].

%% Bias-correct accelerated bootstrap, taken from Bryan O'Sullivan's Criterion
bootstrap_bca(CLevel, Sample, Estimators, Resamples) when CLevel > 0 andalso CLevel < 1 ->
    [e(CLevel, Sample, Est, Resample) || {Est, Resample} <- lists:zip(Estimators, Resamples)].

estimator(mean, Ds) -> eministat:mean(Ds);
estimator(variance, Ds) -> eministat:variance(Ds);
estimator(std_dev, Ds) -> eministat:std_dev(Ds).

e(CLevel, Sample, Est, #dataset { n = N, points = Ps }) ->
    PT = estimator(Est, Sample),
    
    Z1 = quantile(standard(), (1 - CLevel) / 2),
    CumN = fun(X) -> round(N * cumulative(standard(), X)) end,

    ProbN = count(fun(X) -> X < PT end, Ps),
    Bias = quantile(standard(), ProbN / N),
    
    #dataset { points = JackPs } = Jack = jackknife(Est, Sample),
    JackMean = eministat:mean(Jack),
    F = fun(J, {S, C}) ->
        D = JackMean - J,
        D2 = D * D,
        {S + D2, C + D2 * D}
    end,
    {SumSquares, SumCubes} = lists:foldl(F, {0.0,0.0}, JackPs),
    io:format("JackMean: ~p, Jack: ~p~n", [JackMean, Jack]),
    Accel = SumCubes / (6 * (math:pow(SumSquares, 1.5))),

    B1 = Bias + Z1,
    A1 = Bias + B1 / (1 - Accel * B1),
    Lo = max(0, CumN(A1)),
    
    B2 = Bias - Z1,
    A2 = Bias + B2 / (1 - Accel * B2),
    Hi = min(N - 1, CumN(A2)),
    
    #{ pt => PT, lo => Lo, hi => Hi, cl => CLevel }.

quantile(#{ mean := M }, 0.5) -> M;
quantile(#{ mean := M, cdf_denom := CDF }, P) when P > 0 andalso P < 1 ->
    X = inv_erfc(2 * (1 - P)),
    X * CDF + M.

jackknife(Ty, #dataset{ name = N } = Ds) ->
    eministat:ds_from_list({jack, N}, jackknife_(Ty, Ds)).

jackknife_(mean, #dataset { n = N, points = Ps }) when N > 1 ->
    L = N-1,
    [(X + Y) / L || {X, Y} <- zip(prefix_sum_l(Ps), prefix_sum_r(Ps))];
jackknife_(variance, Ds) -> jackknife_variance(0, Ds);
%jackknife_(unbiased_variance, Ds) -> jackknife_variance(1, Ds);
jackknife_(std_dev, Ds) -> [math:sqrt(X) || X <- jackknife_variance(1, Ds)].

jackknife_variance(C, #dataset { n = N, points = Ps } = Ds) when N > 1 ->
    M = eministat:mean(Ds),
    GOA = fun(X) ->
        V = X - M,
        V*V
    end,
    ALs = prefix_sum_l([GOA(P) || P <- Ps]),
    ARs = prefix_sum_r([GOA(P) || P <- Ps]),
    BLs = prefix_sum_l([P - M || P <- Ps]),
    BRs = prefix_sum_r([P - M || P <- Ps]),
    Q = N - 1,
    [begin B = BL + BR, (AL + AR - (B * B) / Q) / (Q - C) end ||
    		{AL, AR, BL, BR} <- zip4(ALs, ARs, BLs, BRs)].

prefix_sum_l(Points) -> scanl(fun erlang:'+'/2, 0.0, Points).
prefix_sum_r(Points) -> tl(scanr(fun erlang:'+'/2, 0.0, Points)).

%% -- NORMAL DISTRIBUTION ------------------------------

%% Constants
sqrt2() -> math:sqrt(2).
sqrt2pi() -> math:sqrt(2 * math:pi()).

standard() ->
    #{ mean => 0.0, std_dev => 1.0, pdf_denom => math:log(sqrt2pi()), cdf_denom => sqrt2() }.

cumulative(#{ mean := M, cdf_denom := CDF}, X) ->
    math:erfc(((M - X) / CDF) / 2).

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

%% These variants of zip ignore extra arguments
zip([X|Xs], [Y|Ys]) -> [{X,Y} | zip(Xs, Ys)];
zip(_, _) -> [].

zip4([A|As], [B|Bs], [C|Cs], [D|Ds]) -> [{A,B,C,D} | zip4(As, Bs, Cs, Ds)];
zip4(_, _, _, _) -> [].

inv_erfc(P) when P > 0 andalso P < 2 ->
    PP = case P =< 1 of
        true -> P;
        false -> 2 - P
    end,
    T = math:sqrt(-2 * math:log(0.5 * PP)),
    %% Initial guess for searching
    X0 = -0.70711 * ((2.30753 + T * 0.27061) / (1 + T * (0.99229 + T * 0.04481)) - T),
    R = inv_erfc_loop(PP, 0, X0),
    case P =< 1 of
        true -> R;
        false -> -R
    end.

inv_erfc_loop(_PP, J, X) when J >= 2 -> X;
inv_erfc_loop(PP, J, X) ->
    Err = math:erfc(X - PP),
    XP = X + Err / (1.12837916709551257 * math:exp(-X * X) - X * Err), %% // Halley
    inv_erfc_loop(PP, J+1, XP).
