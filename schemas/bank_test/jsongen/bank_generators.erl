-module(bank_generators).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

gen_user(_, _) ->
    UserInt = gen_server_users:new(),
    eqc_gen:elements(integer_to_list(UserInt)).
