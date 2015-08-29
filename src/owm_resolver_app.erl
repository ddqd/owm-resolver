-module(owm_resolver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

dispatch_rules() ->
    cowboy_router:compile([
        {'_', [
            % Static("img"),
            {"/api/:param", owm_resolver_http_handler, []},
        	{"/index.html", cowboy_static, {file, "priv/html/index.html"}},
            {"/", cowboy_static, {file, "priv/html/index.html"}},
            {'_', notfound_handler, []}
        ]}
    ]).

getArgs() ->
    _Host = application:get_env(owm_resolver, host),
    {ok, Host} = _Host,
    _Res = application:get_env(owm_resolver, start_type),
    Res = case _Res of 
        {ok, production} ->
            {start_type, production};
        _ ->
            {start_type, testing}
    end,
    [{host, Host}, Res].
    
start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(owm_resolver, port),
	Config = {port, Port},
	Dispatch = dispatch_rules(),
	_CowboyStart = cowboy:start_http(http, 100, [Config], [
		{env, [{dispatch, Dispatch}]}
	]),
	lager:log(info, self(), "owm resolver started on ~p:~p", [node(), Port]),
	owm_resolver_sup:start_link(getArgs()).

stop(_State) ->
    ok.
