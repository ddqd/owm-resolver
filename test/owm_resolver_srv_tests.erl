-module(owm_resolver_srv_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

-define(TMP_FILE, "cities_tmp.txt").

-define(START_STOP_PAUSE, 1000).

start() ->
    Apps = [
            syntax_tools, 
            compiler, 
            goldrush, 
            asn1,
            jsx,
            public_key,
            ssl,
            inets,
            ranch, 
            cowlib, 
            cowboy, 
            mnesia],
    [ok = application:start(A) || A <- Apps],
    ok = application:load(owm_resolver),
    application:set_env(owm_resolver, start_type, test),
    application:start(owm_resolver),
    Pid = whereis(owm_resolver_srv),
    timer:sleep(1000),
    Pid.

stop(_) ->
    Apps = [
            syntax_tools, 
            compiler, 
            goldrush, 
            asn1,
            jsx,
            public_key,
            ssl,
            inets,
            ranch, 
            cowlib, 
            cowboy, 
            mnesia
            ],
    [application:stop(A) || A <- lists:reverse(Apps)],
    application:stop(owm_resolver),
    application:unload(owm_resolver),
    timer:sleep(1000).

start_stop_test_() ->
    [{"The server can be started, stopped and has a registered name ",
     ?setup(fun is_registered/1)},
        {"Test set environment for testing ",
     ?setup(fun test_env/0)}
        ].

is_registered(Pid) ->
    [?_assertEqual(Pid, whereis(owm_resolver_srv))].

test_env() ->
    Res = application:get_env(owm_resolver, start_type),
    [?_assertEqual({ok, test}, Res)].

parse_cities_test_() ->
    {"Parse loaded cities list ",
     ?setup(fun parse_cities/0)}.

parse_cities() ->
    ParsedCitiesList = [error,
                     {819827,"Razvilka",55.591667,37.740833,"RU"},
                     {524901,"Moscow",55.75222,37.615555,"RU"},
                     {1271881,"Firozpur Jhirka",27.799999,76.949997,"IN"},
                     {1283240,"Kathmandu",27.716667,85.316666,"NP"}],
    {ok, Source} = file:read_file("../test/cities_example.txt"),
    ListSource = binary:bin_to_list(Source),
    Result = owm_resolver_srv:parse_city_list(ListSource),
    [?_assertEqual(ParsedCitiesList, Result)].

load_cities_list_test_() ->
    [{"Load cities list from OpenWeatherMap server",
      ?setup(fun test_load/1)}].

test_load(_) ->
	{ok, Data} = owm_resolver_srv:load(),
    os:cmd("rm "?TMP_FILE),
    file:write_file(?TMP_FILE, io_lib:fwrite("~s~n", [Data])),
    IsExist = filelib:is_regular(?TMP_FILE),
	[?_assertEqual(true, IsExist)].

