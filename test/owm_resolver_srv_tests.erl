-module(owm_resolver_srv_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

-define(TMP_FILE, "cities_tmp.txt").

start() ->
    ok = application:start(inets),
    {ok, Pid} = openweather_resolver_srv:start_link(),
    Pid.

stop(_) ->
    openweather_resolver_srv:stop(),
    application:stop(inets).

start_stop_test_() ->
    {"The server can be started, stopped and has a registered name",
     ?setup(fun is_registered/1)}.

% !disabled
load_cities_list_test_() ->
    [{"Load cities list from openweathermap server",
      ?setup(fun test_load/1)}].

parse_cities_test_() ->
    {"Parse loaded cities list",
     ?setup(fun parse_cities/1)}.

is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(openweather_resolver_srv))].

test_load(_) ->
	{ok, Data} = openweather_resolver_srv:load(),
    os:cmd("rm "?TMP_FILE),
    file:write_file(?TMP_FILE, io_lib:fwrite("~s~n", [Data])),
    IsExist = filelib:is_regular(?TMP_FILE),
	[?_assertEqual(true, IsExist)].

parse_cities(_) ->
    ParsedCitiesList = [error,
                     {819827,"Razvilka",55.591667,37.740833,"RU"},
                     {524901,"Moscow",55.75222,37.615555,"RU"},
                     {1271881,"Firozpur Jhirka",27.799999,76.949997,"IN"},
                     {1283240,"Kathmandu",27.716667,85.316666,"NP"}],
    Result = openweather_resolver_srv:readlines("../test/cities_example.txt"),
    [?_assertEqual(ParsedCitiesList, Result)].
