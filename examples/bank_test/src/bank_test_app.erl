%%%-------------------------------------------------------------------
%% @doc bank_test public API
%% @end
%%%-------------------------------------------------------------------

-module(bank_test_app).

-behaviour(application).

%% Application callbacks
-export([start/2, test/0, stop/1, create_first_user/0]).
-import(c, [cd/1, cmd/1]).
%% -import(bank_generators, [gen_user/2]).
%%====================================================================
%% API
%%====================================================================

gen_user(_, _) ->
    UserInt = gen_server_users:new(),
    eqc_gen:elements(integer_to_list(UserInt)).

start(_StartType, _StartArgs) ->
    bank_test_sup:start_link().

test() ->
    {ok, _} = create_first_user(),
    c:cd("jsongen"),
    gen_server_users:start(),
    js_links_machine:run_statem(["new_user.jsch"]),
    cd("..").

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
create_first_user() ->
    httpc:request(post, {"http://localhost:5000/bank/users/", [],
                         "application-json",
                         "{\"user\": \"user0\", \"password\": \"1234\"}"},
                  [], []).
