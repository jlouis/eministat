-module(function_exported).

-export([t/0, datasets/0]).

-define(ROUNDS, 10000).


v1() ->
    v1(?ROUNDS).

v1(0) -> ok;
v1(N) ->
    lists:member({not_a_function, 1}, ?MODULE:module_info(exports)),
    v1(N-1).

v2() ->
    v2(?ROUNDS).

v2(0) -> ok;
v2(N) ->
    try ?MODULE:not_a_function()
    catch
        error:undef ->
            not_defined
    end,
    v2(N-1).

v3() ->
    v3(?ROUNDS).

v3(0) -> ok;
v3(N) ->
    _ = erlang:function_exported(?MODULE, not_a_function, 1),
    v3(N-1).

datasets() ->
    [eministat:s("Variant 1",
                 fun v1/0, 50),
     eministat:s("Variant 2",
                 fun v2/0, 50),
     eministat:s("Variant 3",
                 fun v3/0, 50)].

t() ->
    [H|T] = datasets(),
    eministat:x(95.0, H, T).


