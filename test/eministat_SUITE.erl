-module(eministat_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

-ifdef(EQC).
-define(EQC_PRESENT, true).
-else.
-define(EQC_PRESENT, false).
-endif.

all() -> 
    [ligustrum,
     reptiles,

     bug_000].


groups() -> [].

suite() ->
	[{timetrap, {minutes, 2}}].

init_per_suite(Config) ->
     case ?EQC_PRESENT of
       false ->
	application:load(sasl),
	application:set_env(sasl, sasl_error_logger, false),
	application:set_env(sasl, errlog_type, error),
	error_logger:tty(false),
	ok = application:start(sasl),
	{ok, _} = application:ensure_all_started(eministat),
	Config;
      true ->
        {skip, running_eqc}
    end.

end_per_suite(_Config) ->
	application:stop(eministat),
	application:stop(sasl),
	ok.

init_per_group(_Group, Config) ->
	Config.

end_per_group(_Group, _Config) ->
	ok.

%% Tests.
ligustrum(_Config) ->
    ct:log("Run a simple Eministat test over ligustrum blade thickness"),
    Sun = eministat_ts:ligustrum_sun(),
    Shade = eministat_ts:ligustrum_shade(),
    
    ok = eministat:x(95.0, Sun, Shade),
    ok.

reptiles(_Config) ->
    ct:log("Run a simple Eminsitat test over Iguanas and Chameleons"),

    Chameleon = eministat_ts:chameleon(),
    Iguana = eministat_ts:iguana(),

    ok = eministat:x(95.0, Chameleon, Iguana),

    ok.

bug_000(_Config) ->
    D1 = eministat_ds:from_list(foldl, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]),
    D2 = eministat_ds:from_list(recursion, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]),
    
    ok = eministat:x(95.0, D1, D2),
    ok.
    
%% -- INTERNAL FUNCTIONS ------------------------------------
fact1(N) ->
    lists:foldl(fun(X, Acc) -> X * Acc end, 1, lists:seq(1, N)).

fact2(0) -> 1;
fact2(N) when N > 0 -> N * fact2(N - 1).
