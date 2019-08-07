%%%
%%% Default event handler
-module(erl_health_event_handler).

%% API
-behaviour(erl_health).
-export([handle_event/2]).

%%
-spec handle_event(erl_health:event(), _Opts) ->
    _.
handle_event({Name, started}, _) ->
    logger:debug(
        "Healthcheck ~p started", [Name],
        #{healthcheck => #{name => Name, event => started}}
    );
handle_event({Name, {finished, {Status, Details}}}, _) ->
    Level = case Status of
        passing  -> info;
        warning  -> warning;
        critical -> error
    end,
    logger:Level(
        "Healthcheck ~p: ~p", [Name, Status],
        #{healthcheck => #{name => Name, event => finished, status => Status, details => Details}}
    );
handle_event({Name, {failed, {Class, Reason, Stacktrace}}}, _) ->
    logger:error(
        "Healthcheck ~p failed: ~p:~p ~s",
        [Name, Class, Reason, genlib_format:format_stacktrace(Stacktrace, [newlines])],
        #{healthcheck => #{name => Name, event => failed, error => #{
            class => Class,
            reason => Reason
        }}}
    ).
