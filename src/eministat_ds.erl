%% @doc Common operations on data sets
-module(eministat_ds).

-include("eministat.hrl").

%% -- CONSTRUCTION ----
-export([from_list/2]).

%% -- QUERY ----
-export([
	min/1, max/1,
	median/1,
	mean/1,
	variance/1,
	std_dev/1,
	percentile/2
]).

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

%% -- BASIC QUERIES over #dataset{}'s ----

min(#dataset { points = Ps }) -> float(hd(Ps)).

max(#dataset { points = Ps }) -> float(lists:last(Ps)).

median(Ds) -> percentile(0.5, Ds).

mean(#dataset { n = N, sy = SY }) -> float(SY / N).

variance(#dataset { n = N, sy = SY, syy = SYY }) ->
    (SYY - SY * SY / N) / (N - 1.0).

std_dev(Ds) -> math:sqrt(variance(Ds)).

percentile(P, #dataset { n = N, points = Ps }) ->
    float(lists:nth(round(N * P), Ps)).
