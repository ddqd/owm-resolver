-module(openweather_resolver_srv).
 
-behaviour(gen_server).
 
-include("openweather_resolver.hrl").

-export([start_link/0, stop/0]).
 
-export([init/1]).
 
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([load/0, readlines/1]).

-define(CITIES_URL, "http://openweathermap.org/help/city_list.txt").

load() ->
	R = httpc:request(get, {?CITIES_URL, []}, [], []),
	case R of
		{ok, {{_HTTP_VERSION, _ReturnCode, _State}, _Head, Body}} ->
			{ok, Body};
		_ ->
			{error, "Unable to load"}
	end.

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
      after file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line ->
            [H|_T] = parse_cities_lines(Line),
            Tokens = parse_city_line(H),
            [parse_city(Tokens) | get_all_lines(Device)]
    end.

parse_city_line(CityString) ->
    string:tokens(CityString, "\t").

parse_cities_lines(Line) ->
    string:tokens(Line, "\n").

parse_city([Id, Name, Lat, Lon, Code]) ->
    case string:to_integer(Id) of 
        {Id2, []} ->
            {Lat2, []} = string:to_float(Lat),
            {Lon2, []} = string:to_float(Lon),
            {Id2, Name, Lat2, Lon2, Code};
        _ ->
            error
        end.

init([]) ->
	lager:log(info, self(), "resolver started"),
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
 
handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.