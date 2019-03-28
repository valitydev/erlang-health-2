-module(erl_health_handle).

%% API
-export([get_route/1]).

%% cowboy_handler callbacks
-behaviour(cowboy_handler).

-export([init/2, terminate/3]).

-type checkers() :: [erl_health:checker()].

%%
%% API
%%
-spec get_route(checkers()) ->
    {iodata(), module(), checkers()}.
get_route(Checkers) ->
    {"/health", ?MODULE, Checkers}.

%%
%% cowboy_http_handler callbacks
%%
-spec init(cowboy_req:req(), checkers()) ->
    {ok, cowboy_req:req(), checkers()}.
init(Req0, Checkers) ->
    {Code, Headers, RespBody} =
        case erl_health:check(Checkers) of
            {ok, RespJSON} ->
                Headers_ = #{<<"Content-Type">> => <<"application/json">>, <<"Cache-Control">> => <<"no-cache">>},
                {200, Headers_, jsx:encode(RespJSON)};
            {error, Code_, Msg}->
                {Code_, #{<<"Content-Type">> => <<"text/plain">>}, Msg}
        end,
    Req = cowboy_req:reply(Code, Headers, RespBody, Req0),
    {ok, Req, Checkers}.

-spec terminate(_Reason, cowboy_req:req(), checkers()) ->
    ok.
terminate(_, _, _) ->
    ok.

