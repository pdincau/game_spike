-module(player).
-behaviour(gen_server).

-export([start_link/0, stop/0, move/1, current_position/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PlayerKey, {player, move}).

-record(state, {position}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

move(Direction) ->
    gen_server:cast(?MODULE, {move, Direction}).

current_position() ->
    gen_server:call(?MODULE, current_position).

init([]) ->
    Position = world:assign_position(),
    {ok, #state{position=Position}}.

handle_call(current_position, _From, #state{position=Position} = State) ->
    {reply, Position, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({move, Direction}, #state{position=Position} = State) ->
    NewPosition = move(Direction, Position),
    io:format("Player new position is: ~p~n", [NewPosition]),
    NewState = State#state{position=NewPosition},
    gproc:send({p, l, ?PlayerKey}, {?PlayerKey, NewPosition}),
    {noreply, NewState};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

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

