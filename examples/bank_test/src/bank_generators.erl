-module(bank_generators).

-include_lib("eqc/include/eqc.hrl").

-import(eqc_gen, [binary/0]).
-export([gen_user/2, gen_password/2]).

gen_user(_, _) ->
  UserInt = gen_server_users:new(),
  eqc_gen:elements([UserInt]).

%% TODO implement better password generator
gen_password(_, _) ->
  %% eqc_gen:elements([integer_to_list(1234)]).
  ?SUCHTHAT(Xs, eqc_gen:utf8(), size(Xs) > 10).
