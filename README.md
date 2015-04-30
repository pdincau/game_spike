game_spike
=====

An OTP application

Build
-----

    $ ./rebar3 compile
    $ erl -pa _build/default/lib/*/ebin
    1> [application:start(X) || X <- [crypto, ranch, cowlib, cowboy, jsx, gproc, game_spike]].
