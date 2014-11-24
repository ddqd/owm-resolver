-module(openweather_resolver_srv).
 
-behaviour(gen_server).
 
-export([start_link/0, stop/0]).
 
-export([init/1]).
 
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([load/0]).

-define(CITIES_URL, "http://openweathermap.org/help/city_list.txt").

load() ->
	R = httpc:request(get, {?CITIES_URL, []}, [], []),
	case R of
		{ok, {{_HTTP_VERSION, _ReturnCode, _State}, _Head, Body}} ->
			{ok, Body};
		_ ->
			{error, "Unable to load"}
	end.

init([]) ->
	lager:log(info, self(), "resolver started"),
    {ok, state}.

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