-module(bank_generators).

-include_lib("eqc/include/eqc.hrl").

-import(eqc_gen, [list/1]).
-export([gen_user/2, gen_password/2]).

gen_user(_, _) ->
    UserInt = gen_server_users:new(),
    eqc_gen:elements(integer_to_list(UserInt)).

gen_password(_, _) ->
    ?SUCHTHAT(Xs, eqc_gen:char(), length(Xs) > 10).
