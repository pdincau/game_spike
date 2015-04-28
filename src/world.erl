-module(world).
-export([assign_position/0, handle_move/2]).

assign_position() ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    X = random:uniform(26) + 2,
    Y = random:uniform(24) + 2,
    {n, X, Y}.

handle_move({0, -1}, {_, X, 2}) ->
    {s, X, 2};

handle_move({0, -1}, {_, X, Y}) ->
    {s, X, Y-1};

handle_move({-1, 0}, {_, 2, Y}) ->
    {w, 2, Y};

handle_move({-1, 0}, {_, X, Y}) ->
    {w, X-1, Y};

handle_move({0, 1}, {_, X, 26}) ->
    {n, X, 26};

handle_move({0, 1}, {_, X, Y}) ->
    {n, X, Y+1};

handle_move({1, 0}, {_, 28, Y}) ->
    {e, 28, Y};

handle_move({1, 0}, {_, X, Y}) ->
    {e, X+1, Y}.

