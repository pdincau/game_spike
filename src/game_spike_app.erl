-module(game_spike_app).
-behaviour(application).

-export([start/2
        ,stop/1]).

start(_StartType, _StartArgs) ->
    start_cowboy(),
    monster:start_link(),
    game_spike_sup:start_link().

stop(_State) ->
    ok.

start_cowboy() ->
    TransOpts = [{port, 8080}],
    Routes = routes(),
    Dispatch = cowboy_router:compile(Routes),
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _} = cowboy:start_http(http, 100, TransOpts, ProtoOpts).

routes() ->
    [{'_', [{"/ws", ws_handler, []}]}].
