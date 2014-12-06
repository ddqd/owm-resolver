-module(owm_resolver_http_handler).

-export([
    init/3
  ]).

-behaviour(cowboy_resource_handler).

-include("owm_resolver.hrl").

-export([
    allowed/2,
    authorize/3,
    call/3,
    delete/2,
    get/2,
    patch/3,
    post/3,
    put/3
  ]).

init(_Transport, _Req, _Options) ->
    {upgrade, protocol, cowboy_resource}.

authorize(_Type, _Credentials, _Options) ->
    {ok, {<<"">>, <<"">>}}.

allowed(_, none) ->
    true;

allowed(_Method, {_Identity, _Scope}) ->
    true.

%===================================

get(Query, _Options) ->
    case proplists:get_value(param, Query) of
        <<"search">> ->
            parse_search(Query);
        <<"cc">> ->
            get_countries();
        _ ->
            {ok, []}
    end.

post(_Entity, _Query, _Options) ->
    {ok, []}.

put(_Entity, _Query, _Options) ->
    {ok, []}.

patch(_Changes, _Query, _Options) ->
    {ok, []}.

delete(_Query, _Options) ->
    {ok, []}.

call(_, _, _) ->
    {ok, []}.

%===================================

parse_search(Query) ->
    Name = getValueFromQuery(<<"name">>, Query),
    CountryCode = getValueFromQuery(<<"countryCode">>, Query),
    Format = getFormatFromQuery(Query),
    {ok, Result} = owm_resolver_db:search(Name, CountryCode),
    JsonAnswer = city_list_to_json(Result, Format),
    % lager:log(info, self(), "search result ~p", [JsonAnswer]),
    {ok, [<<"ok">>, JsonAnswer]}.

getValueFromQuery(Key, Query) ->
    case proplists:get_value(Key, Query) of
        undefined ->
            [];
        Result ->
            binary:bin_to_list(Result)
    end.

getFormatFromQuery(Query) ->
    FormatList = getValueFromQuery(<<"format">>, Query),
    case FormatList of
        "short" ->
            fun (#cities{id = Id, name = Name, code = Code})  ->
                    jsx:encode([{<<"id">>, erlang:integer_to_binary(Id)},
                                {<<"name">>, erlang:list_to_binary(Name)},
                                {<<"countryCode">>, erlang:list_to_binary(Code)}])
            end;
        _ ->
            fun (#cities{id = Id, name = Name, lat = Lat, lon = Lon, code = Code}) ->
                    jsx:encode([{<<"id">>, erlang:integer_to_binary(Id)},
                                {<<"name">>, erlang:list_to_binary(Name)},
                                {<<"lat">>, erlang:float_to_binary(Lat, [{decimals, 6}])},
                                {<<"lon">>, erlang:float_to_binary(Lon, [{decimals, 6}])},
                                {<<"countryCode">>, erlang:list_to_binary(Code)}])
            end
    end.

city_list_to_json(CityList, FormatFun) ->
  city_list_to_json(CityList, FormatFun, []).

city_list_to_json([], _FormatFun, Acc) ->
  Acc;

city_list_to_json([H|T], FormatFun, Acc) ->
    Result = FormatFun(H),
    city_list_to_json(T, FormatFun, Acc++[Result]).

get_countries() ->
    {ok, Result} = owm_resolver_db:get_countries(),
    case Result of
        [] ->
           {ok, []};
        R ->
            BinResult = lists:map(fun erlang:list_to_binary/1, R),
            {ok, [{result, BinResult}]}
    end.
   
