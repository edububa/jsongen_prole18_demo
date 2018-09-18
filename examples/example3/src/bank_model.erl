-module(bank_model).

-export([initial_state/0, next_state/4, postcondition/4]).

-record(state, {users, accounts}).
-record(info, {op_title, call_body, priv_state, json_res}).

initial_state() ->
  #state
    {
     users = [],
     accounts = #{}
    }.

next_state(Super, State, Result, Call) ->
  Info = get_info(Call, State, Result),
  NextModelState = next_model_state(Info#info.op_title,
                                    Info#info.priv_state,
                                    Info#info.call_body,
                                    Info#info.json_res),
  NewState = jsg_links_utils:set_private_state(NextModelState, State),
  Super(NewState, Result, Call).

postcondition(Super, State, Call, Result) ->
  Info = get_info(Call, State, Result),
  postcondition_model_state(Info#info.op_title,
                            Info#info.priv_state,
                            Info#info.json_res)
    and Super(State, Call, Result).


%% auxiliar functions

json_call_body([_,{_, _, BodyArg, _,_}]) ->
  case BodyArg of
    {ok,Body} -> Body;
    undefined -> {struct, []}                  % GET
  end.

get_info(Call, State, Result) ->
  #info{
     op_title   = js_links_machine:call_link_title(Call),
     call_body  = json_call_body(Call),
     priv_state = jsg_links_utils:private_state(State),
     json_res   = js_links_machine:get_json_body(Result)
    }.

%% model change

next_model_state(Operation, ModelState, {struct,BodyValues}, {struct,Values}) ->
  case Operation of
    "new_user" ->
      case proplists:lookup(<<"user">>, Values) of
        {_, User} ->
          ModelState#state {
            users = [User|ModelState#state.users]
           };
        none -> ModelState
      end;
    "new account" ->
      case {proplists:lookup(<<"accountid">>, Values), proplists:lookup(<<"balance">>, Values)} of
        {{_, AccountId}, {_, Balance}} ->
          ModelState#state {
            accounts = maps:put(AccountId, Balance, ModelState#state.accounts)
           };
        _ -> ModelState
      end;
    "deposit" ->
      case proplists:lookup(<<"accountid">>, Values) of
        {_, AccountId} ->
          ModelState#state {
            accounts = maps:put(AccountId,
                                maps:get(AccountId, ModelState#state.accounts) +
                                  proplists:get_value(<<"quantity">>, BodyValues),
                                ModelState#state.accounts)
           };
        _ -> ModelState
      end;
    "withdraw" ->
      case {proplists:lookup(<<"accountid">>, Values), proplists:lookup(<<"balance">>, Values)} of
        {{_, AccountId}, {_, _Balance}} ->
          ModelState#state {
            accounts = maps:put(AccountId,
                                maps:get(AccountId, ModelState#state.accounts) -
                                  proplists:get_value(<<"quantity">>, BodyValues),
                                ModelState#state.accounts)
           };
        _ -> ModelState
      end;
    _ -> ModelState
  end.

%% model check
postcondition_model_state(Operation, ModelState, {struct, Values}) ->
  NegativeAccounts
    = maps:filter(fun(AccountId, Balance) ->
                      Balance < 0
                  end, ModelState#state.accounts),
  case maps:keys(NegativeAccounts) of
    [] -> true;
    Accounts ->
      print_error(Accounts),
      false
  end and
    case Operation of
      "consult account" ->
        case {proplists:lookup(<<"accountid">>, Values),
              proplists:lookup(<<"balance">>, Values)} of
          {{_, AccountId}, {_, Balance}} ->
            Balance == maps:get(AccountId, ModelState#state.accounts);
          _ -> false
        end;
      _ -> true
    end.

%% postcondition_model_state(Operation, ModelState, {struct, Values}) ->
%%   maps:keys(maps:filter(fun(AccountId, Balance) ->
%%                             Balance < 0
%%                         end, ModelState#state.accounts)) == []
%%     and case Operation of
%%           "consult account" ->
%%             case {proplists:lookup(<<"accountid">>, Values),
%%                   proplists:lookup(<<"balance">>, Values)} of
%%               {{_, AccountId}, {_, Balance}} ->
%%                 Balance == maps:get(AccountId, ModelState#state.accounts);
%%               _ -> false
%%             end;
%%           _ -> true
%%         end.


print_error(Accounts) ->
  io:format("~n***************************************************~n" ++
              "ERROR [postcondition error][negative balance]: ~nAccountId: ~p" ++
              "~n***************************************************~n",
            [Accounts]).

%% [{_,[_, {_, HRef}, _, _, _, {_, _, BodyArg, _, _}]
