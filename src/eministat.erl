-module(eministat).

-include("eministat.hrl").

-export([
         s/3, s/4,
         x/3,
         a/3
]).

-define(ROUNDS, 10000). %% Bootstrap resampling count

valid_ci(80.0) -> 1;
valid_ci(90.0) -> 2;
valid_ci(95.0) -> 3;
valid_ci(98.0) -> 4;
valid_ci(99.0) -> 5;
valid_ci(99.5) -> 6;
valid_ci(_) -> error(badarg).

vitals_bootstrapped(#dataset { n = N } = Ds, CI) ->
    Bootstrapped = eministat_resample:resample([mean, std_dev], ?ROUNDS, Ds),
    vitals_outliers(Ds, #{
        n => N,
        ci => CI,
        min => eministat_ds:min(Ds),
        max => eministat_ds:max(Ds),
        percentiles =>
            maps:from_list([{P, eministat_ds:percentile(P / 100, Ds)} || P <- [25, 50, 75, 90, 95, 99, 99.5, 99.9] ]),
        bootstrapped =>
            maps:from_list(eministat_resample:bootstrap_bca(CI / 100, Ds, Bootstrapped))
    }).

vitals_outliers(#dataset { points = Ps }, #{ percentiles := #{ 25 := Q1, 75 := Q3 } } = Data) ->
    IQR = Q3 - Q1,
    Data#{ outliers =>
        {eministat_resample:count(fun(P) -> P < (Q1 - 1.5*IQR) end, Ps),
         eministat_resample:count(fun(P) -> P > (Q3 + 1.5*IQR) end, Ps)}}.

x(CI, #dataset{} = Ds0, #dataset{} = Ds1) ->
    x(CI, Ds0, [Ds1]);
x(CI, #dataset{} = Ds0, L) when is_list(L) ->
    case lists:all(fun(D) -> is_record(D, dataset) end, L) of
        true ->
            valid_ci(CI),
            h([Ds0 | L]),
            eministat_plot:p(74, [Ds0 | L]),
            r(CI, Ds0, L);
        false ->
            error(badarg)
    end.

h(DSs) -> h(DSs, 1).

h([], _) -> ok;
h([#dataset { name = Name } | Next], Symb) ->
    io:format("~s ~s\n", [eministat_report:symbol(Symb), Name]),
    h(Next, Symb+1).

a(CI, Ds, DSs) ->
    V1 = vitals_bootstrapped(Ds, CI),
    io:format("------\n\n"),

    #{ dataset => Ds,
       vitals => V1,
       relative => a(CI, Ds, DSs, 1)
     }.

a(CI, Ds, [Rs | Next], I) ->
    M = #{
      dataset => Ds,
      vitals => vitals_bootstrapped(Rs, CI),
      relative => eministat_analysis:relative(Rs, Ds, valid_ci(CI))
     },
    
    [M | a(CI, Ds, Next, I+1)];
a(_, _, [], _) -> [].

o(#{ vitals := V, relative := R}) ->
    eministat_report:vitals(V, 1),
    io:format("------\n\n"),
    o_relative(R, 2).

o_relative([#{ vitals := Vs, relative := R } | Next], K) ->
    eministat_report:vitals(Vs, K),
    eministat_report:relative(R),
    io:format("------\n\n"),
    o_relative(Next, K+1);
o_relative([], _) -> ok.


r(CI, Ds, DSs) ->
    o(a(CI, Ds, DSs)).

s(Name, F, Samples) -> s(Name, F, Samples, us).

s(Name, F, Samples, us) -> s(Name, F, Samples, fun(X) -> X end);
s(Name, F, Samples, ms) -> s(Name, F, Samples, fun(X) -> X div 1000 end);
s(Name, F, Samples, s) -> s(Name, F, Samples, fun(X) -> X div (1000*1000) end);
s(Name, F, Samples, G) when is_function(G, 1) ->
    eministat_ds:from_list(Name, s_run(F, G, Samples)).
    
s_run(F, G, K) ->
    s_warmup(F),
    s_bench(F, G, K).

s_warmup(F) ->
    erlang:send_after(3000, self(), warmup_over),
    s_warmup_loop(F).
    
s_warmup_loop(F) ->
    receive
        warmup_over -> ok
    after 0 ->
        F(),
        F(),
        F(),
        s_warmup_loop(F)
    end.

s_bench(_F, _G, 0) -> [];
s_bench(F, G, K) ->
    garbage_collect(),
    [G(element(1, timer:tc(F))) | s_bench(F,G, K-1)].
