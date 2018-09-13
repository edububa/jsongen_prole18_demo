-export([initial_state/0, next_state/4, postcondition/4]).

initial_state() ->
  ...

next_state(Super, State, Result, Call) ->
  ...

postcondition(Super, State, Call, Result) ->
  ...
