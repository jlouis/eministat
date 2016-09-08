-module(maps_get).

-export([datasets/0, t/0, control/2]).

without(0, _) -> ok;
without(K, Map) ->
    K1 = {a, 1},
    K2 = {a, 2},
    K3 = {a, 3},
    #{K1 := V1, K2 := V2, K3 := V3} = Map,
    {V1, V2, V3},
    without(K-1, Map).

with(0, _) -> ok;
with(K, Map) ->
    {maps:get({a,1}, Map),
     maps:get({a,2}, Map),
     maps:get({a,3}, Map)},
    with(K-1, Map).

control(0, _) -> ok;
control(K, Map) ->
    control(K-1, Map).

datasets() ->
    Map = #{{a, 1} => 100, {a, 2} => 200, {a, 3} => 300},
    [eministat:s("without", fun() -> without(10000, Map) end, 25, us),
     eministat:s("with", fun() -> with(10000, Map) end, 25, us),
     eministat:s("control", fun() -> control(10000, Map) end, 25, us)].

t() ->
    [Without | RelativeTo] = datasets(),
    eministat:x(95.0, Without, RelativeTo).
