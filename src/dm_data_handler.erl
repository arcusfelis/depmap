-module(dm_data_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).
-compile({parse_transform, seqbind}).

%% ------------------------------------------------------------------
%% Callbacks
%% ------------------------------------------------------------------

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req@, State) ->
    {Path, Req@} = cowboy_http_req:path_info(Req@),
    case Path of
        [<<"deps.gexf">>] ->
            {ok, Req@} = cowboy_http_req:reply(200, [], generate_xml(), Req@),
            {ok, Req@, State}
    end.

terminate(_Req, _State) ->
    ok.

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

generate_xml() ->
    FileName = filename:join(code:priv_dir(gitto), "reps.bin"),
    {ok, RepsBin} = file:read_file(FileName),
    Xml = gitto_deps_gexf:build_xml(binary_to_term(RepsBin)),
    gexf:to_string(Xml).

