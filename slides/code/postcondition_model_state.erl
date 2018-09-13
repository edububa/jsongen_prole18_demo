postcondition_model_state(Operation, ModelState,
                          {struct, Values}) ->
  case Operation of
    "consult account" ->
      case {proplists:lookup(<<"accountid">>, Values),
            proplists:lookup(<<"balance">>, Values)} of
        {{_, AccountId}, {_, Balance}} ->
          Balance == maps:get(AccountId,
                              ModelState#state.accounts);
        _ -> false
      end;
    _ -> true
  end.
