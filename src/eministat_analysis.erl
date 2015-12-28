-module(eministat_analysis).

-export([outlier_variance/3]).

%% @doc outlier_variance/3 computes the severity of the outlier variance
outlier_variance(_, Sigma, _) when Sigma < 0.000000000000001 -> {0.0, unaffected};
outlier_variance(Mu, Sigma, A) ->
    MinBy = fun(F, Q, R) -> min(F(Q), F(R)) end,

    MuA = Mu / A,
    MugMin = MuA / 2,
    SigmaG = min(MugMin / 4, Sigma / math:sqrt(A)),
    SigmaG2 = SigmaG * SigmaG,
    Sigma2 = Sigma * Sigma,
    
    VarOut = fun(C) ->
        AC = A - C,
        (AC / A) * (Sigma2 - AC * SigmaG2)
    end,
    
    CMax = fun(X) ->
        K = MuA - X,
        D = K * K,
        AD = A * D,
        K0 = -A * AD,
        K1 = Sigma2 - A * SigmaG2 + AD,
        Det = K1 * K1 - 4 * SigmaG2 * K0,
        trunc(-2 * K0 / (K1 + math:sqrt(Det)))
    end,

    VarOutMin = MinBy(VarOut, 1, (MinBy(CMax, 0, MugMin))) / Sigma2,
    case VarOutMin of
        K when K < 0.01 -> {K, unaffected};
        K when K < 0.1 -> {K, slight};
        K when K < 0.5 -> {K, moderate};
        K when K > 0.5 -> {K, severe}
    end.
