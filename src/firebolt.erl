-module(firebolt).
-behaviour(gen_server).

-export([start/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 125).
-define(FireboltKey, {firebolt, move}).

-record(state, {position, step, tref}).

start(Position) ->
    gen_server:start(?MODULE, [Position], []).

init([Position]) ->
    %%TODO: should i cancel timer?
    {ok, TRef} = timer:send_interval(?TIMEOUT, timeout),
    {ok, #state{position=Position, step=0, tref=TRef}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(move, #state{step=6, tref=TRef} = State) ->
    timer:cancel(TRef),
    {stop, normal, State};

handle_cast(move, #state{position=Position, step=Step} = State) ->
    {Direction, _, _} = Position,
    NewPosition = move(Direction, Position),
    NewState =  State#state{position=NewPosition, step=Step+1},
    gproc:send({p, l, ?FireboltKey}, {?FireboltKey, NewPosition}),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    gen_server:cast(self(), move),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

move(n, Position) ->
    world:handle_move({0, 1}, Position);

move(s, Position) ->
    world:handle_move({0, -1}, Position);

move(e, Position) ->
    world:handle_move({1, 0}, Position);

move(w, Position) ->
    world:handle_move({-1, 0}, Position).
