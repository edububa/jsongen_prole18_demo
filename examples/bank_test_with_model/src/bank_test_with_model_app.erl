%%%-------------------------------------------------------------------
%% @doc bank_test_with_model public API
%% @end
%%%-------------------------------------------------------------------

-module(bank_test_with_model_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         test/0,
         stop/1]).
-import(c, [cd/1, cmd/1]).
%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  gen_server_users:start(),
  bank_test_with_model_sup:start_link().

test() ->
  c:cd("jsongen"),
  js_links_machine:run_statem(bank_model, ["new_user.jsch"]),
  c:cd("..").

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
