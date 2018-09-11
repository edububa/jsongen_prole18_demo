-module(gen_server_users).

-behaviour(gen_server).

-export([init/1]).
-export([handle_call/3, terminate/2]).
-export([start/0, new/0]).

init(_) -> {ok, 0}.

handle_call(Msg, _From, State) ->
    case Msg of
        {new} ->
            {reply, State, State + 1}
    end.

terminate(_,_) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    {ok, Pid} = gen_server:start(?MODULE,[],[]),
    register(?MODULE, Pid),
    ?MODULE.

new() -> gen_server:call(?MODULE, {new}).
