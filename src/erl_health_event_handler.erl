%%%
%%% Default event handler
-module(erl_health_event_handler).

%% API
-behaviour(erl_health).
-export([handle_event/2]).

%%
-spec handle_event(erl_health:event(), _Opts) ->
    _.
handle_event(Event = {Name, started}, _) ->
    logger:debug(
        "Healthcheck ~p started", [Name],
        #{healthcheck => construct_meta(Event)}
    );
handle_event(Event = {Name, {finished, {Status, _}}}, _) ->
    Level = case Status of
        passing  -> info;
        warning  -> warning;
        critical -> error
    end,
    logger:Level(
        "Healthcheck ~p: ~p", [Name, Status],
        #{healthcheck => construct_meta(Event)}
    );
handle_event(Event = {Name, {failed, {Class, Reason, Stacktrace}}}, _) ->
    logger:error(
        "Healthcheck ~p failed: ~p:~p ~s",
        [Name, Class, Reason, genlib_format:format_stacktrace(Stacktrace, [newlines])],
        #{healthcheck => construct_meta(Event)}
    ).

-spec construct_meta(erl_health:event()) ->
    #{atom() => _}.
construct_meta({Name, Ev = started}) ->
    #{name => Name, event => Ev};
construct_meta({Name, {Ev = finished, {Status, Details}}}) ->
    #{name => Name, event => Ev, status => Status, details => construct_details(Details)};
construct_meta({Name, {Ev = failed, {Class, Reason, _}}}) ->
    #{name => Name, event => Ev, error => #{class => Class, reason => Reason}}.

construct_details(Details = #{}) ->
    Details;
construct_details(Details) ->
    #{value => Details}.
