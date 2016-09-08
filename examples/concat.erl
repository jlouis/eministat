-module(concat).

-export([t/0, datasets/0]).

c1(Items) -> c1(Items, 10000).

c1(_, 0) -> ok;
c1(Items, K) ->
    string:join(Items, ""),
    c1(Items, K-1).

c2(Items) -> c2(Items, 10000).

c2(_, 0) -> ok;
c2(Items, K) ->
    lists:append(Items),
    c2(Items, K-1).

c3(Items) -> c3(Items, 10000).

c3(_, 0) -> ok;
c3([A,B,C] = Is, K) -> 
    _ = A ++ B ++ C,
    c3(Is, K-1).

datasets() ->
    ItemID = "123134-123-12313-1--123-1231-",
    Input = ["Item:{", ItemID, "}"],
    [eministat:s("++",
                 fun() ->
                         c3(Input)
                 end, 50),
     eministat:s("strings:join/1",
                 fun() ->
                         c1(Input)
                 end, 50),
     eministat:s("lists:append",
                 fun() ->
                         c2(Input)
                 end, 50)].

t() ->
    [H | T] = datasets(),
    eministat:x(95.0, H, T).

                                           

