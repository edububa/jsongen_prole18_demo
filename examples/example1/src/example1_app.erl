%%%-------------------------------------------------------------------
%% @doc example1 public API
%% @end
%%%-------------------------------------------------------------------

-module(example1_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  gen_server_users:start(),
  test(),
  example1_sup:start_link().

test() ->
  c:cd("jsongen"),
  js_links_machine:run_statem(["new_user.jsch"]),
  c:cd("..").

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================