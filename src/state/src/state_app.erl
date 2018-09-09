-module(state_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    state_sup:start_link().

stop(_State) ->
    ok.
