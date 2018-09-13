-module(bank_generators).

-include_lib("eqc/include/eqc.hrl").

-import(eqc_gen, [binary/0]).
-export([gen_user/2, gen_password/2, gen_deposit/2, gen_withdraw/2]).

gen_user(_, _) ->
  UserInt = gen_server_users:new(),
  eqc_gen:elements([UserInt]).

gen_password(_, _) ->
  %% eqc_gen:elements([integer_to_list(1234)]).
  ?SUCHTHAT(Xs, eqc_gen:utf8(), size(Xs) > 10).

gen_deposit(_, _) ->
  ?LET(Xs, "deposit", Xs).

gen_withdraw(_, _) ->
  ?LET(Xs, "withdraw", Xs).
