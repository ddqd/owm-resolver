-module(openweather_resolver_srv_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

start_stop_test_() ->
    {"The server can be started, stopped and has a registered name",
     ?setup(fun is_registered/1)}.

load_test_() ->
    [{"Load cities list",
      ?setup(fun test_load/1)}].

start() ->
	ok = application:start(inets),
    {ok, Pid} = openweather_resolver_srv:start_link(),
    Pid.

stop(_) ->
    openweather_resolver_srv:stop(),
	application:stop(inets).

is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(openweather_resolver_srv))].

test_load(_) ->
	{Result, Data} = openweather_resolver_srv:load(),
	?debugFmt("load data ~s~n", [Data]),
	[?_assertEqual(ok, Result)].