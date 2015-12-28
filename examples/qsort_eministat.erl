-module(qsort_eministat).

-export([datasets/0, t/0]).

datasets() ->
    Input = [rand:uniform(10000000) || _ <- lists:seq(1, 100*1000)],
    [eministat:s("sortl", fun() -> qsort:sortl(Input) end, 50, ms),
    eministat:s("sortf", fun() -> qsort:sortf(Input) end, 50, ms),
    eministat:s("sortfl", fun() -> qsort:sortfl(Input) end, 50, ms),
    eministat:s("sortx", fun() -> qsort:sortx(Input) end, 50, ms),
    eministat:s("lists:sort/1", fun() -> lists:sort(Input) end, 50, ms)].
    
t() ->
    [SortL | RelativeTo] = datasets(),
    eministat:x(95.0, SortL, RelativeTo).
