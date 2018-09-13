-record(state, {users, accounts}).

initial_state() ->
  #state
    {
     users = [],
     accounts = #{}
    }.
