next_state(Super, State, Result, Call) ->
  ...
  case Operation of
    "new_user" ->
      case proplists:lookup(<<"user">>, Values) of
        {_, User} ->
          ModelState#state {
            users = [User|ModelState#state.users]
           };
        none -> ModelState
      end;
    ...
