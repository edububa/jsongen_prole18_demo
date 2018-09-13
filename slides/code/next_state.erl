next_state(Super, State, Result, Call) ->
  Info = get_info(Call, State, Result),
  NextModelState = next_model_state(Info#info.op_title,
                                    Info#info.priv_state,
                                    Info#info.call_body,
                                    Info#info.json_res),
  NewState = jsg_links_utils:
    set_private_state(NextModelState, State),
  Super(NewState, Result, Call).
