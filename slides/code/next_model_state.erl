next_model_state(Operation, ModelState, {struct,BodyValues},
                 {struct,Values}) ->
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
