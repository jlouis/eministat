-module(eministat_eqc).
-include_lib("eqc/include/eqc.hrl").

clevel() ->
    elements([80.0, 90.0, 95.0, 98.0, 99.0, 99.5]).

input() -> oneof([integer(), real()]).
           
dataset() ->
    ?LET(Inputs, [input(), input(), input() | list(input())],
         eministat_ds:from_list(
           dummy_ds,
           Inputs)).

crashes(F) ->
    try
        F(), true
    catch
        Class:Reason ->
            {error, Class, Reason}
    end.

prop_total() ->
    ?FORALL([CL, Base, Relatives], [clevel(), dataset(), list(dataset())],
            not crashes(fun() ->
                                eministat:a(CL, Base, Relatives)
                        end)).
