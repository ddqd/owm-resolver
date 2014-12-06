-module(owm_resolver_db).
 
-export([install/0, search/1, search/2, set_data/1, get_all/0, is_exist/0, get_countries/0]).

-include_lib("stdlib/include/qlc.hrl").

-include("owm_resolver.hrl").
 
create_table() ->
    case mnesia:create_table(cities,
                [{attributes, record_info(fields, cities)},
                {index, []},
                {storage_properties,[   
                    {ets, [compressed]}, 
                    {dets, [{auto_save, 3000}]} 
                ]}, {ram_copies, [node()]}]) of
        {aborted, {already_exists, Key}} -> lager:log(info, self(), "Table ~p Already Exist", [Key]);
        {atomic,ok} -> 
            lager:log(info, self(), "Table Created"),
            {ok, created};
        {aborted, Reason} -> 
            lager:log(error, self(), "Create table Error ~p", [Reason]),
            {aborted, Reason}
    end.

install() ->
    mnesia:stop(),
    mnesia_schema:delete_schema([node()]),
    case  mnesia:create_schema([node()]) of 
        ok ->
            mnesia:start(),
            create_table();
        Error -> 
            lager:log(info, self(), "Create schema error ~p", [Error]),
            Error
    end.

search(ReqName, []) ->
    search(ReqName);

search(ReqName, CountryCode) ->
    F = fun() ->
        Query = qlc:q([ Cities || Cities <- mnesia:table(cities), ((is_match(Cities#cities.name, ReqName)) and (Cities#cities.code == CountryCode)) ] ),
        Res = qlc:eval(qlc:sort(Query, {order, fun sort_by_name/2})),
        case Res of 
            [] -> 
                {error, not_found};
            Result ->
                {ok, Result}
        end
    end,
    transaction(F).

search(ReqName) ->
    F = fun() ->
        Query = qlc:q([ Cities || Cities <- mnesia:table(cities), (is_match(Cities#cities.name, ReqName)) ] ),
        Res = qlc:eval(qlc:sort(Query, {order, fun sort_by_name/2})),
        case Res of 
            [] -> 
                {error, not_found};
            Result ->
                {ok, Result}
        end
    end,
    transaction(F).

is_match(CityName, ReqName) ->
    case re:run(CityName, "^"++ReqName, [caseless]) of
        nomatch -> false;
        _ -> true
    end.

sort_by_name(A, B) ->
     sort_by_alphabetical(A#cities.name, B#cities.name).

sort_by_alphabetical(A, B) ->
    A =< B.

set_data(CityList) when is_list(CityList) ->
    mnesia:ets(
        fun()-> 
            [mnesia:dirty_write(
                #cities{id = Id, name = Name, lat = Lat, lon = Lon, code = Code}) || {Id, Name, Lat, Lon, Code} <- CityList] 
        end),
    mnesia:change_table_copy_type(cities, node(), disc_copies).

get_all() ->
    mnesia:transaction( 
        fun() ->
            qlc:eval( qlc:q(
                [ X || X <- mnesia:table(cities)] 
            )) 
    end ).

transaction(F) ->
    mnesia:activity(transaction, F).

is_exist() ->
    Tables = mnesia:system_info(tables),
    lists:member(cities, Tables).

get_countries() ->
     F = fun() ->
        Query = qlc:q([ Code || #cities{code = Code} <- mnesia:table(cities)] ),
        Res = qlc:eval(qlc:sort(Query, {order, fun sort_by_alphabetical/2})),
        case Res of 
            [] -> 
                {error, not_found};
            Result ->
                {ok, Result}
        end
    end,
    transaction(F).
