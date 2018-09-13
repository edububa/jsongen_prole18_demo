-module(bank_model).

-export([initial_state/0, next_state/4, postcondition/4]).

-record(state, {users, accounts}).
-record(info, {op_title, call_body, priv_state, json_res}).

initial_state() ->
    #state{
       users = [],
       accounts = #{}
      }.

next_state(Super, State, Result, Call) ->
    Info = get_info(Call, State, Result),
    NextPrivateState = next_private_state(Info#info.op_title,
                                          Info#info.priv_state,
                                          Info#info.call_body,
                                          Info#info.json_res),
    NewState = jsg_links_utils:set_private_state(NextPrivateState, State),
    Super(NewState, Result, Call).

postcondition(Super, State, Call, Result) ->
    Info = get_info(Call, State, Result),
    postcondition_private_state(Info#info.op_title,
                                Info#info.priv_state,
                                Info#info.json_res)
        and Super(State, Call, Result).

%% auxiliar functions

json_call_body([_,{_, _, BodyArg, _,_}]) ->
    case BodyArg of
        {ok,Body} -> Body;
        undefined -> undefined                  % GET
    end.

get_info(Call, State, Result) ->
    #info{
       op_title   = js_links_machine:call_link_title(Call),
       call_body  = json_call_body(Call),
       priv_state = jsg_links_utils:private_state(State),
       json_res   = js_links_machine:get_json_body(Result)
      }.

next_private_state("new user", PrivateState, CallBody, {struct, Values}) ->
    case proplists:lookup(<<"user">>, Values) of
        {_, User} ->
            PrivateState#state {
              users = [User|PrivateState#state.users]
             };
        none -> PrivateState
    end;
next_private_state("new account", PrivateState, ResultBody, {struct, Values}) ->
    case {proplists:lookup(<<"accountid">>, Values), proplists:lookup(<<"balance">>, Values)} of
        {{_, AccountId}, {_, Balance}} ->
            PrivateState#state {
              accounts = maps:put(AccountId, Balance, PrivateState#state.accounts)
             };
        _ -> PrivateState
    end;
next_private_state("deposit", PrivateState, {struct, BodyValues}, {struct, Values}) ->
    case proplists:lookup(<<"accountid">>, Values) of
        {_, AccountId} ->
            PrivateState#state {
              accounts = maps:put(AccountId,
                                  maps:get(AccountId, PrivateState#state.accounts) +
                                      proplists:get_value(<<"quantity">>, BodyValues),
                                  PrivateState#state.accounts)
             };
        _ -> PrivateState
    end;
next_private_state("withdraw", PrivateState, {struct, BodyValues}, {struct, Values}) ->
    case {proplists:lookup(<<"accountid">>, Values), proplists:lookup(<<"balance">>, Values)} of
        {{_, AccountId}, {_, Balance}} ->
            PrivateState#state {
              accounts = maps:put(AccountId,
                                  maps:get(AccountId, PrivateState#state.accounts) -
                                      proplists:get_value(<<"quantity">>, BodyValues),
                                  PrivateState#state.accounts)
             };
        _ -> PrivateState
    end;
next_private_state(_, PrivateState, _, _) ->
    PrivateState.


postcondition_private_state("consult account", PrivateState, {struct, Values}) ->
    case {proplists:lookup(<<"accountid">>, Values), proplists:lookup(<<"balance">>, Values)} of
        {{_, AccountId}, {_, Balance}} ->
            Balance == maps:get(AccountId, PrivateState#state.accounts, -1);
        _ -> false
    end;
postcondition_private_state(_, _, _) ->
    true.


%% [{_,[LinkType,
%%                     HRef,
%%                     LinkNumber,
%%                     SchemaRef,
%%                     Headers]},
%%                 {RequestLink,
%%                  _,
%%                  Body,
%%                  _,_}]


%% [{_,[_, {_, HRef}, _, _, _, {_, _, BodyArg, _, _}]
