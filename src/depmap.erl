-module(depmap).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(mimetypes),
	application:start(cowboy),
	application:start(jsx),
	application:start(lager),
	application:start(depmap).


start(_Type, _Args) ->
    PrivDir = code:priv_dir(depmap),
    JsDir = abs_path(filename:join([PrivDir, "js"])),
    HtmlDir = abs_path(filename:join([PrivDir, "html"])),
    FontDir = abs_path(filename:join([PrivDir, "font"])),
    StaticFilesCfg = [{mimetypes, {fun mimetypes:path_to_mimes/2, default}}],

	Dispatch = [
		{'_', [
			{[<<"data">>, '...'], dm_data_handler, []},
			{[], dm_default_handler, []},

            {[<<"js">>, '...'], cowboy_http_static,
                 [{directory, JsDir}|StaticFilesCfg]},

            {[<<"font">>, '...'], cowboy_http_static,
                 [{directory, FontDir}|StaticFilesCfg]},

            {['...'], cowboy_http_static,
                 [{directory, HtmlDir}|StaticFilesCfg]}
		]}
	],
	cowboy:start_listener(depmap_http, 100,
		cowboy_tcp_transport, [{port, 2080}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
%   cowboy:start_listener(https, 100,
%   	cowboy_ssl_transport, [
%   		{port, 1443}, {certfile, "priv/ssl/cert.pem"},
%   		{keyfile, "priv/ssl/key.pem"}, {password, "cowboy"}],
%   	cowboy_http_protocol, [{dispatch, Dispatch}]
%   ),
	dm_sup:start_link().

stop(_State) ->
	ok.


%%
%% Private
%%

abs_path(Path) -> 
    filename:join(
        abs_path_(
            filename:split(
                filename:absname(Path)), [])).


abs_path_([".."|T], [_|Stack]) ->
    abs_path_(T, Stack);
abs_path_([H|T], Stack) ->
    abs_path_(T, [H|Stack]);
abs_path_([], Stack) ->
    lists:reverse(Stack).
