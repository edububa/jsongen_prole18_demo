postcondition_state(Super, State, Call, Result) ->
  ...
  NegativeAccounts = maps:keys(
                       maps:filter(fun(AccountId, Balance) ->
                                       Balance < 0
                                   end,
                                   ModelState#state.accounts)),
  (NegativeAccounts == []) and
    case Operation of
      "balance account" ->
        case {proplists:lookup(<<"accountid">>, Values),
              proplists:lookup(<<"balance">>, Values)} of
          {{_, AccountId}, {_, Balance}} ->
            Balance == maps:get(AccountId,
                                ModelState#state.accounts);
          _ -> false
        end;
      _ -> true
    end.
