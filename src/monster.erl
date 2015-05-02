-module(monster).
-behaviour(gen_server).

-export([start_link/0, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 250).
-define(MonsterKey, {monster, move}).
-define(FireboltKey, {firebolt, move}).

-record(state, {position, path, energy, tref}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

init([]) ->
    gproc:reg({p, l, ?FireboltKey}),
    {ok, TRef} = timer:send_interval(?TIMEOUT, timeout),
    {ok, #state{position={n, 2, 2}, path=path(), energy=2, tref=TRef}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(move, #state{position=Position, path=[Direction|Directions]} = State) ->
    NewPath = Directions ++ [Direction],
    NewPosition = move(Direction, Position),
    io:format("Monster new position is: ~p~n", [NewPosition]),
    NewState = State#state{position=NewPosition, path=NewPath},
    gproc:send({p, l, ?MonsterKey}, {?MonsterKey, NewPosition}),
    {noreply, NewState};

handle_cast(stop, #state{tref=TRef} = State) ->
    timer:cancel(TRef),
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({?FireboltKey, FireboltPosition}, State) ->
    handle_maybe_hit(FireboltPosition, State);

handle_info(timeout, State) ->
    gen_server:cast(?SERVER, move),
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

handle_maybe_hit(FireboltPosition, #state{position=Position} = State) ->
    case collision(FireboltPosition, Position) of
        true ->
            handle_maybe_dead(State);
        false ->
            {noreply, State}
    end.

collision({_, X, Y}, {_, X, Y}) ->
    true;

collision(_, _) ->
    false.

handle_maybe_dead(#state{energy=0} = _State) ->
    {stop, normal, noreply};

handle_maybe_dead(#state{energy=Energy} = State) ->
    NewState = State#state{energy=Energy-1},
    {noreply, NewState}.
