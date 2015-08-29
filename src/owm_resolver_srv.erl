-module(owm_resolver_srv).
 
-behaviour(gen_server).
 
-include("owm_resolver.hrl").

-export([start_link/1, start_link/2, start_link/3, stop/0]).
 
-export([init/1]).
 
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([load/1, parse_city_list/1]).

-record(state, {host=[]}).

load(Host) ->
	R = httpc:request(get, {Host, []}, [], []),
	case R of
		{ok, {{_HTTP_VERSION, _ReturnCode, _State}, _Head, Body}} ->
			{ok, Body};
		_ ->
			{error, "Unable to load"}
	end.


parse_city_list(Data) ->
    CityLines = parse_cities_lines(Data),
    parse_city_line(CityLines).

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

init(Args_) ->
    {args, Args} = Args_,
	lager:log(info, self(), "resolver started with mode ~p", [Args]),
    Host =  proplists:get_value(host, Args),
    Type = proplists:get_value(start_type, Args),
    lager:log(info, self(), "start update ~p", [Type]),
    check_load(),
    {ok, #state{host=Host}}.

check_load() ->
    case owm_resolver_db:is_exist() of
        false ->
            lager:log(info, self(), "start update", []),
            gen_server:cast(?MODULE, load);
        _ ->
            lager:log(info, self(), "db is updated", []),
            ok
    end.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(load, State) ->
    _Result = owm_resolver_db:install(),
    lager:log(info, self(), "Loading city_list", []),
   case load(State#state.host) of
        {ok, Body} ->
            CityList = parse_city_list(Body),
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
 
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

start_link(Args, _) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

start_link(Args, State, _) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, State).

stop() ->
    gen_server:call(?MODULE, stop).
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.