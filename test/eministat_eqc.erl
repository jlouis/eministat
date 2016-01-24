-module(eministat_eqc).
-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

clevel() ->
    elements([80.0, 90.0, 95.0, 98.0, 99.0, 99.5]).

input() -> oneof([choose(1,1000), real()]).
           
dataset(K) ->
    ?LET([Prefix, L], [vector(K, input()), list(input())],
       eministat_ds:from_list(dummy_ds, lists:append(Prefix, L))).
           
dataset() ->
    ?LET(K, nat(),
        dataset(K+3)).

total(F) ->
    try
        F(), true
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

prop_total() ->
    ?FORALL([CL, Base, Relatives], [clevel(), dataset(), list(dataset())],
            total(fun() ->
                                eministat:a(CL, Base, Relatives)
                        end)).
