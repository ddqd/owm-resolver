-module(owm_resolver_db_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("owm_resolver.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

-define(TEST_CITY_RECORD, [{17, "City17", 36.3456, -34.5324, "NA"}]).

start() ->
    Apps = [syntax_tools, 
            compiler, 
            goldrush, 
            asn1,
            jsx,
            public_key,
            ssl,
            lager,
            inets,
            ranch, 
            cowlib, 
            cowboy, 
            mnesia],
    [ok = application:start(A) || A <- Apps],
    install(),
    ok = application:load(owm_resolver),
    application:set_env(owm_resolver, start_type, test),
    application:start(owm_resolver),
    Pid = whereis(owm_resolver_srv),
    timer:sleep(1000),
    Pid.

stop(_) ->
    mnesia:delete_table(cities),
    Apps = [syntax_tools, 
            compiler, 
            goldrush, 
            asn1,
            jsx,
            public_key,
            ssl,
            lager,
            inets,
            ranch, 
            cowlib, 
            cowboy, 
            mnesia],
    [application:stop(A) || A <- lists:reverse(Apps)],
    application:stop(owm_resolver),
    application:unload(owm_resolver),
    timer:sleep(1000).

start_stop_test_() ->
    [{"The server can be started, stopped and has a registered name",
     ?setup(fun is_registered/1)}].

is_registered(Pid) ->
    [?_assertEqual(Pid, whereis(owm_resolver_srv))].

db_test_() ->
    [
    {"Test write city record to mnesia",
     ?setup(fun write_record/0)},
    {"Test search city by name", ?setup(fun search_city/0)},
    {"Test get codes of countries ",?setup(fun get_countries/0)}
        ].

install() ->
    Result = owm_resolver_db:install(),
    [?_assertEqual({ok, created}, Result)].

write_record() ->
    WriteResult = owm_resolver_db:set_data(?TEST_CITY_RECORD),
    [?_assertEqual({ok, created}, WriteResult)].

search_city() ->
    write_record(),
    Result = owm_resolver_db:search("Ci"),
    lager:log(info, self(), "SearchResult ~p", [Result]), 
    lager:log(info, self(), "get all ~p", [owm_resolver_db:get_all()]),
    [?_assertEqual({ok, ?TEST_CITY_RECORD}, Result)].     

get_countries() ->
    TestData = [
                    {17, "Bark", 36.3456, -34.5324, "AB"},
                    {99, "Woof", 11.22, -23.44, "CD"},
                    {12, "Meow", 99.99, 99, "EF"}
                ],
    ResultExpected = ["AB", "CD", "EF"],
    SetDataResult = owm_resolver_db:set_data(TestData), 
    {ok, Res} = owm_resolver_db:get_countries(),
    CompareResult = compare_lists(ResultExpected, Res),
    [
        ?_assertEqual({atomic, ok}, SetDataResult),
        ?_assert(CompareResult)
    ].

compare_lists(List1, List2) ->
    if (erlang:length(List1) =/= erlang:length(List1)) -> false;
        true ->
            compare_lists(lists:sort(List1), lists:sort(List2), [])
    end.

compare_lists([], [], _Acc) ->
    true;

compare_lists([H|T], [H2|T2], Acc) ->
    case string:equal(H, H2) of
        true ->
            compare_lists(T, T2, Acc);
        false ->
            false
    end;

compare_lists(_,_,_) ->
    false.