-module(ws_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).

-define(MonsterKey, {monster, move}).
-define(PlayerKey, {player, move}).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_handle({text, <<"start">>}, Req, State) ->
    gproc:reg({p, l, ?MonsterKey}),
    gproc:reg({p, l, ?PlayerKey}),
    {ok, _} = player:start_link(),
    {Direction, X, Y} = player:current_position(),
    Reply = jsx:encode([{<<"position">>,[{<<"direction">>, Direction}, {<<"x">>, X}, {<<"y">>, Y}]}]),
    {reply, {text, Reply}, Req, State};

websocket_handle({text, Msg}, Req, State) ->
    handle_msg(Msg),
    {ok, Req, State};

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({?MonsterKey, {Direction, X, Y}}, Req, State) ->
    Reply = jsx:encode([{<<"mposition">>,[{<<"direction">>, Direction}, {<<"x">>, X}, {<<"y">>, Y}]}]),
    {reply, {text, Reply}, Req, State};

websocket_info({?PlayerKey, {Direction, X, Y}}, Req, State) ->
    Reply = jsx:encode([{<<"position">>,[{<<"direction">>, Direction}, {<<"x">>, X}, {<<"y">>, Y}]}]),
    {reply, {text, Reply}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    player:stop(),
    ok.

handle_msg(<<"{\"move\":\"up\"}">>) ->
    player:move(n);

handle_msg(<<"{\"move\":\"down\"}">>) ->
    player:move(s);

handle_msg(<<"{\"move\":\"left\"}">>) ->
    player:move(w);

handle_msg(<<"{\"move\":\"right\"}">>) ->
    player:move(e).
