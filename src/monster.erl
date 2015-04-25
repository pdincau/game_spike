-module(monster).
-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 250).
-define(MonsterKey, {monster, move}).

-record(state, {position, path}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([]) ->
    %%TODO: should i cancel timer?
    timer:send_interval(?TIMEOUT, timeout),
    {ok, #state{position={n, 2, 2}, path=path()}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(move, #state{position=Position, path=[Direction|Directions]} = State) ->
    NewPath = Directions ++ [Direction],
    NewPosition = move(Direction, Position),
    io:format("Monster new position is: ~p~n", [NewPosition]),
    NewState = State#state{position=NewPosition, path=NewPath},
    gproc:send({p, l, ?MonsterKey}, {?MonsterKey, NewPosition}),
    {noreply, NewState};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    gen_server:cast(monster, move),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

path() ->
    [n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n,
     s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s].

move(n, Position) ->
    world:handle_move({0, 1}, Position);

move(s, Position) ->
    world:handle_move({0, -1}, Position);

move(e, Position) ->
    world:handle_move({1, 0}, Position);

move(w, Position) ->
    world:handle_move({-1, 0}, Position).
