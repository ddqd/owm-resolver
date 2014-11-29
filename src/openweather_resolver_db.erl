-module(openweather_resolver_db).
 
-export([install/0, search/1, set_data/1, get_all/0]).

-include_lib("stdlib/include/qlc.hrl").

-include("openweather_resolver.hrl").
 
%% Internal functions

create_table() ->
    case mnesia:create_table(cities,
                [{attributes, record_info(fields, cities)},
                {index, []},
                {storage_properties,[   
                    {ets, [compressed]}, 
                    {dets, [{auto_save, 3000}]} 
                ]}, {disc_copies, [node()]}]) of
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

search(Name) ->
    IsMatch = fun(Str) ->
        case re:run(Str, "^"++Name) of
                nomatch -> false;
                _ -> true
            end
    end,
    F = fun() ->
        Query = qlc:q([ Cities || Cities <- mnesia:table(cities), (IsMatch(Cities#cities.name)) ] ),
        Res = qlc:e(Query),
        case Res of 
            [] -> 
                {error, not_found};
            [Cities|_] ->
                {ok, Cities}
        end
    end,
    transaction(F).

set_data({Id, Name, Lat, Lon, Code}) ->
    F = fun() ->
        case mnesia:write(#cities{id = Id, name = Name, lat = Lat, lon = Lon, code = Code}) of 
            ok -> {ok, success};
            _ -> {error, write_error}
        end
    end,
    transaction(F).

get_all() ->
    mnesia:transaction( 
        fun() ->
            qlc:eval( qlc:q(
                [ X || X <- mnesia:table(cities)] 
            )) 
    end ).

transaction(F) ->
    mnesia:activity(transaction, F).


