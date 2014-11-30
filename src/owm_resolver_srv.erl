-module(owm_resolver_srv).
 
-behaviour(gen_server).
 
-include("owm_resolver.hrl").

-export([start_link/0, stop/0]).
 
-export([init/1]).
 
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([load/0]).

-define(CITIES_URL, "http://openweathermap.org/help/city_list.txt").

-record(state, {param}).

load() ->
	R = httpc:request(get, {?CITIES_URL, []}, [], []),
	case R of
		{ok, {{_HTTP_VERSION, _ReturnCode, _State}, _Head, Body}} ->
			{ok, Body};
		_ ->
			{error, "Unable to load"}
	end.

parse_city_line(Lines) ->
    parse_city_line(Lines, []).

parse_city_line([], Acc) ->
    Acc;

parse_city_line([H|T], Acc) ->
    Line = string:tokens(H, "\t"),
    CityLine = parse_city(Line),
    case CityLine of 
        error ->
            parse_city_line(T, Acc);
        _ ->
            parse_city_line(T, Acc++[CityLine])
    end.

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
        end;

parse_city(Other) ->
    lager:log(info, self(), "error parse city ~p", [Other]),
    error.

init([]) ->
	lager:log(info, self(), "resolver started"),
    case owm_resolver_db:is_exist() of
        true ->
            {ok, #state{}};
        false ->
            gen_server:cast(?MODULE, load),
            {ok, #state{}}
    end.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(load, State) ->
    Result = owm_resolver_db:install(),
    lager:log(info, self(), "Loading city_list", []),
   case load() of
        {ok, Body} ->
            CityLines = parse_cities_lines(Body),
            CityList = parse_city_line(CityLines),
            lager:log(info, self(), "list is ~p", [CityList]),
            owm_resolver_db:set_data(CityList); 
        {error, _Message} ->
            error
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    lager:log(info, self(), "handle_info ~p", [State]),
    {noreply, State}.
 
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.