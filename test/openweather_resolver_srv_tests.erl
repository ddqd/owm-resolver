-module(openweather_resolver_srv_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

succeed() ->
    ok.

succeeding_test() ->
    succeed().

succeeding_fun_test_() ->
    fun () -> succeed() end.