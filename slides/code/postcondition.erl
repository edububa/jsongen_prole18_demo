postcondition(Super, State, Call, Result) ->
  Info = get_info(Call, State, Result),
  postcondition_model_state(Info#info.op_title,
                            Info#info.priv_state,
                            Info#info.json_res)
    and Super(State, Call, Result).
