...
"new account" ->
   case {proplists:lookup(<<"accountid">>, Values),
         proplists:lookup(<<"balance">>, Values)} of
     {{_, AccountId}, {_, Balance}} ->
       ModelState#state {
         accounts = maps:put(AccountId,
                             Balance,
                             ModelState#state.accounts)
        };
     _ -> ModelState
   end;
 ...
