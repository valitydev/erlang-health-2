-module(erl_health_handle).

%% API
-export([get_route/1]).
-export([get_liveness_route/1]).
-export([get_readiness_route/1]).

%% cowboy_handler callbacks
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

%%
%% API
%%
-spec get_route(erl_health:check()) ->
    {iodata(), module(), erl_health:check()}.
get_route(Check) ->
    {"/health", ?MODULE, Check}.

-spec get_liveness_route(erl_health:check()) ->
    {iodata(), module(), erl_health:check()}.
get_liveness_route(Check) ->
    {"/healthz/liveness", ?MODULE, Check}.

-spec get_readiness_route(erl_health:check()) ->
    {iodata(), module(), erl_health:check()}.
get_readiness_route(Check) ->
    {"/healthz/readiness", ?MODULE, Check}.

%%
%% cowboy_http_handler callbacks
%%
-spec init(cowboy_req:req(), erl_health:check()) ->
    {ok, cowboy_req:req(), erl_health:check()}.
init(Req0, Check) ->
    {Status, Details} = erl_health:check(Check),
    %% > https://www.consul.io/api/agent/check.html#http
    %% If the response is any 2xx code, the check is `passing`. If the response is `429 Too Many Requests`,
    %% the check is `warning`. Otherwise, the check is `critical`.
    Code = case Status of
        passing  -> 200;
        warning  -> 429;
        critical -> 503
    end,
    Headers = #{
        <<"Content-Type">>  => <<"application/json">>,
        <<"Cache-Control">> => <<"no-cache">>
    },
    Req1 = cowboy_req:reply(Code, Headers, jsx:encode(Details), Req0),
    {ok, Req1, Check}.

-spec terminate(_Reason, cowboy_req:req(), erl_health:check()) ->
    ok.
terminate(_, _, _) ->
    ok.

