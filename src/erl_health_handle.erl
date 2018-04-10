-module(erl_health_handle).

%% API
-export([get_route/1]).

%% cowboy_http_handler callbacks
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

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
-spec init({_, http}, cowboy_req:req(), checkers()) ->
    {ok, cowboy_req:req(), checkers()}.
init({_Transport, http}, Req, Checkers) ->
    {ok, Req, Checkers}.

-spec handle(cowboy_req:req(), checkers()) ->
    {ok, cowboy_req:req(), checkers()}.
handle(Req, Checkers) ->
    {Code, Headers, RespBody} =
        case erl_health:check(Checkers) of
            {ok, RespJSON} ->
                Headers_ = [{<<"Content-Type">>, <<"application/json">>}, {<<"Cache-Control">>, <<"no-cache">>}],
                {200, Headers_, jsx:encode(RespJSON)};
            {error, Code_, Msg}->
                {Code_, [{<<"Content-Type">>, <<"text/plain">>}], Msg}
        end,
    {ok, NewReq} = cowboy_req:reply(Code, Headers, RespBody, Req),
    {ok, NewReq, Checkers}.

-spec terminate(_Reason, cowboy_req:req(), checkers()) ->
    ok.
terminate(_, _, _) ->
    ok.

