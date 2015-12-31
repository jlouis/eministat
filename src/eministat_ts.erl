%%% @doc Various test datasets
%%% @end
-module(eministat_ts).

-export([
	chameleon/0, iguana/0,
	ligustrum_shade/0, ligustrum_sun/0,
	reverse_1/0, reverse_2/0
]).

%% -- CHAMELEON/IGUANA from ministat(1) ------------------------------------
%%
chameleon() ->
    eministat_ds:from_list("chameleon", [150, 400, 720, 500, 930]).

iguana() ->
    eministat_ds:from_list("iguana", [50, 200, 150, 400, 750, 400, 150]).

%% -- LIGUSTRUM ---------------------------------------------
%%
ligustrum_sun() ->
    eministat_ds:from_list("sun", [150, 100, 210, 300, 200, 210, 300]).

ligustrum_shade() ->
    eministat_ds:from_list("shade", [120, 125, 160, 130, 200, 170, 200]).

%% -- lists:reverse/1 benchmarks -----------------------------------
%%
reverse_1() ->
    L = lists:seq(1, 1000000),
    eministat:s("lists:reverse/1", fun() -> lists:reverse(L) end, 10, us).

reverse_2() ->
    L = lists:seq(1, 1000000),
    eministat:s("tail_reverse/1", fun() -> tail_reverse(L) end, 15, us).

tail_reverse(L) -> tail_reverse(L, []).

tail_reverse([], Acc) -> Acc;
tail_reverse([X | Xs], Acc) -> tail_reverse(Xs, [X | Acc]).
