-module(bank_model).

-compile(export_all).

inital_state() ->
    #{}.

next_state(Super, State, Result, Call) ->
    Super(State, Result, Call).

postcondition(Super, State, Call, Result) ->
    Super(State, Call, Result).
