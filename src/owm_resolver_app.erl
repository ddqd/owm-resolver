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
    Res = application:get_env(owm_resolver, start_type),
    case Res of 
        {ok, production} ->
            [{start_type, production}];
        _ ->
            [{start_type, testing}]
    end.
    
start(_StartType, _StartArgs) ->
    {ok, _Port} = application:get_env(owm_resolver, port),
	Config = {port, _Port},
	{_, Port} = Config, 
	Dispatch = dispatch_rules(),
	{ok, _} = cowboy:start_http(http, 100, [Config], [
		{env, [{dispatch, Dispatch}]}
	]),
	lager:log(info, self(), "owm resolver started on ~p:~p", [node(), Port]),
	owm_resolver_sup:start_link(getArgs()).

stop(_State) ->
    ok.
