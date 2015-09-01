-module(sm_gcm_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(_, _) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    sm_gcm_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
    ok.
