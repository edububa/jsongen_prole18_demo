-module(bank_config).

-export([
         dispatch/0,
         web_config/0
        ]).

-spec dispatch() -> [webmachine_dispatcher:route()].
dispatch() ->
    lists:flatten([
                   {["bank"], auth_resource, [new]},
                   {["bank", user], bank_resource, [new]},
                   {["bank", user, account], bank_resource, [other]}
                  ]).

web_config() ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Ip} = application:get_env(App, web_ip),
    {ok, Port} = application:get_env(App, web_port),
    [
     {ip, Ip},
     {port, Port},
     {log_dir, "priv/log"},
     {dispatch, dispatch()}
    ].
