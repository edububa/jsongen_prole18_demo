-module(typechecking_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    typechecking_sup:start_link().

stop(_State) ->
    ok.
