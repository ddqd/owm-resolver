-module(owm_resolver_db_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("owm_resolver.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

-define(TEST_CITY_RECORD, {17, "City17", 36.3456, -34.5324, "NA"}).

start() ->
    ok = application:start(mnesia),
    mnesia:clear_table(cities),
    {ok, Pid} = owm_resolver_srv:start_link(),
    Pid.

stop(_) ->
    owm_resolver_srv:stop(),
    application:stop(mnesia).

start_stop_test_() ->
    {"The server can be started, stopped and has a registered name",
     ?setup(fun is_registered/1)}.

is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(owm_resolver_srv))].

db_tests_() ->
    [{"Test create mnesia table",
     ?setup(fun install/0)}, 
    {"Test write city record to mnesia",
     ?setup(fun write_record/0)},
    {"Test search city by name",
     ?setup(fun search_city/0)}].

install() ->
    Result = owm_resolver_db:install(),
    [?_assertEqual({ok, created}, Result)].

write_record() ->
    owm_resolver_db:install(),
    WriteResult = owm_resolver_db:set_data(?TEST_CITY_RECORD),
    [?_assertEqual({ok, created}, WriteResult)].

search_city() ->
    write_record(),
    Result = owm_resolver_db:search("Ci"),
    [?_assertEqual({ok, ?TEST_CITY_RECORD}, Result)].     
