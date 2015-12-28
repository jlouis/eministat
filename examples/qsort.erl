-module(qsort).

-export([sortl/1, sortf/1, sortfl/1, sortx/1]).

-compile(inline_list_funcs).
-compile({inline,[lesser_and_bigger2/3, lesser_and_bigger3/3]}).


%%
%% sort using list:reverse
divide_to_lesser_and_bigger1(List, X) ->
  divide_to_lesser_and_bigger1(List, X, [], []).

divide_to_lesser_and_bigger1([H|L], X, Lesser, Bigger) when H < X ->
  divide_to_lesser_and_bigger1(L, X, [H|Lesser], Bigger);
divide_to_lesser_and_bigger1([H|L], X, Lesser, Bigger) when H >= X ->
  divide_to_lesser_and_bigger1(L, X, Lesser, [H|Bigger]);
divide_to_lesser_and_bigger1([], _, Lesser, Bigger) ->
  [lists:reverse(Lesser)|lists:reverse(Bigger)].

sortl([]) ->
  [];
sortl([H|L]) ->
  [Lesser|Bigger] = divide_to_lesser_and_bigger1(L, H),
  sortl(Lesser) ++ [H] ++ sortl(Bigger).


%%
%% sort using foldr
divide_to_lesser_and_bigger2(List, X) ->
   lists:foldr(fun(E, Acc) -> lesser_and_bigger2(E, X, Acc) end, {[], []}, List).

lesser_and_bigger2(E, X, {Lesser, Bigger}) when E < X ->
   {[E|Lesser], Bigger};
lesser_and_bigger2(E, _, {Lesser, Bigger}) ->
   {Lesser, [E|Bigger]}.

sortf([]) ->
  [];
sortf([H|L]) ->
  {Lesser, Bigger} = divide_to_lesser_and_bigger2(L, H),
  sortf(Lesser) ++ [H] ++ sortf(Bigger).


%%
%% sort using foldr (and list as accumulator)
divide_to_lesser_and_bigger3(List, X) ->
   lists:foldr(fun(E, Acc) -> lesser_and_bigger3(E, X, Acc) end, [[]], List).

lesser_and_bigger3(E, X, [Lesser|Bigger]) when E < X ->
   [[E|Lesser]|Bigger];
lesser_and_bigger3(E, _, [Lesser|Bigger]) ->
   [Lesser|[E|Bigger]].

sortfl([]) ->
  [];
sortfl([H|L]) ->
  [Lesser|Bigger] = divide_to_lesser_and_bigger3(L, H),
  sortfl(Lesser) ++ [H] ++ sortfl(Bigger).


%%
%% sort without list:reverse
divide_to_lesser_and_bigger4([H|L], X) when H < X ->
   [Lesser|Bigger] = divide_to_lesser_and_bigger4(L, X),
   [[H|Lesser]|Bigger];
divide_to_lesser_and_bigger4([H|L], X) when H >= X ->
   [Lesser|Bigger] = divide_to_lesser_and_bigger4(L, X),
   [Lesser|[H|Bigger]];
divide_to_lesser_and_bigger4([], _) ->
   [[]].

sortx([]) ->
  [];
sortx([H|L]) ->
  [Lesser|Bigger] = divide_to_lesser_and_bigger4(L, H),
  sortx(Lesser) ++ [H] ++ sortx(Bigger).


