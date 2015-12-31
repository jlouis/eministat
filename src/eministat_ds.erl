%% @doc Common operations on data sets
-module(eministat_ds).

-include("eministat.hrl").

-export([from_list/2]).

from_list(Name, Points) ->
    F = fun(P, {X, XX, K}) -> {X + P, XX + (P * P), K+1} end,
    {SY, SYY, N} = lists:foldl(F, {0.0, 0.0, 0}, Points),
    case N < 3 of
        true ->
            error(dataset_too_small);
        false ->
            #dataset {
                name = Name,
                points = lists:sort(Points),
                sy = SY,
                syy = SYY,
                n = N
            }
    end.
