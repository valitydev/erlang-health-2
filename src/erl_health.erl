%%%
%%% Стандартные проверки лимитов
%%% Логика работы следующая:
%%%  - если чекер запускается, то нужно добавить запись в результат;
%%%  - если есть лимит в опциях, то нужно проверить его и в случае превышения отдать 503
%%%
-module(erl_health).

%% API
-export([check/1]).

-export([cpu      /1]).
-export([load     /1]).
-export([memory   /1]).
-export([cg_memory/1]).
-export([disk     /2]).
-export([service  /1]).

-export_type([check/0]).
-export_type([status /0]).
-export_type([details/0]).
-export_type([result /0]).
-export_type([checker/0]).
-export_type([event/0]).

%%
%% API
%%
-type status()  :: passing | warning | critical.
-type details() :: number() | binary() | list() | map().

-type result()           :: {status(), #{name() => details()}}.
-type check_runner()     :: {module(), atom(), list()} | fun(() -> check_run_result()).
-type check_run_result() :: {status(), details()}.

-type checker() :: check_runner() | #{
    runner        := check_runner(),
    event_handler => event_handler()
}.

-type name()  :: atom().
-type check() :: #{name() => checker()}.

%% Event handler
-type event() ::
    {name(),
        started               |
        {finished , result()} |
        {failed   , _Error}
    }.

-type event_handler() :: {module(), _Opts}.

-callback handle_event(event(), _Opts) ->
    _.

%%
-spec check(check()) ->
    result().
check(Check) ->
    Initial = {passing, #{}},
    maps:fold(fun (Name, C, Acc) -> compose(Acc, Name, run_checker(Name, C)) end, Initial, Check).

-spec run_checker(name(), checker()) ->
    check_run_result().
run_checker(Name, Checker = #{runner := Runner}) ->
    _ = emit_event({Name, started}, Checker),
    try
        Result = call_checker(Runner),
        _ = emit_event({Name, {finished, Result}}, Checker),
        Result
    catch Class:Reason:Stacktrace ->
        _ = emit_event({Name, {failed, {Class, Reason, Stacktrace}}}, Checker),
        erlang:raise(Class, Reason, Stacktrace)
    end;
run_checker(Name, Runner) ->
    run_checker(Name, #{runner => Runner}).

-spec compose(result(), name(), check_run_result()) ->
    result().
compose({StatusAcc, DetailsAcc}, Name, {Status, Details}) ->
    {worst_status(StatusAcc, Status), DetailsAcc#{Name => Details}}.

-spec worst_status(status(), status()) ->
    status().
worst_status(Status   , Status ) -> Status;
worst_status(warning  , passing) -> warning;
worst_status(critical , passing) -> critical;
worst_status(critical , warning) -> critical;
worst_status(S1, S2 ) when
    S1 == passing;
    S1 == warning;
    S1 == critical
->
    worst_status(S2, S1).

-spec call_checker(check_runner()) ->
    result().
call_checker({M, F, A}) ->
    erlang:apply(M, F, A);
call_checker(Checker) ->
    Checker().

-spec emit_event(event(), checker()) ->
    _ | no_handler.
emit_event(Event, #{event_handler := {Module, Opts}}) ->
    Module:handle_event(Event, Opts);
emit_event(_Event, _Checker) ->
    no_handler.

%%

%% cpu utilization limit
%% cpu_sup:util()
-spec cpu(number()) ->
    result().
cpu(Limit) ->
    limit(cpu_sup:util(), Limit, #{}).

%% load limit
%% cpu_sup:avg1()
-spec load(number()) ->
    result().
load(Limit) ->
    limit(cpu_sup:avg1(), Limit, #{}).

%% memory limit
%% memsup:get_system_memory_data(), (total - free) / total
-spec memory(number()) ->
    result().
memory(Limit) ->
    % > http://erlang.org/doc/man/memsup.html#get_system_memory_data-0
    % On linux the memory available to the emulator is `cached_memory` and `buffered_memory`
    % in addition to free_memory.
    #{
        free_memory     := Free,
        cached_memory   := Cached,
        buffered_memory := Buffered,
        total_memory    := Total
    } = maps:from_list(memsup:get_system_memory_data()),
    TotalFree = Free + Cached + Buffered,
    Details = #{free => TotalFree, total => Total},
    limit((Total - TotalFree) * 100 div Total, Limit, Details).

%% cgroups memory limit
%% /sys/fs/cgroups memory.stat->rss / memory.limit_in_bytes
-spec cg_memory(number()) ->
    result().
cg_memory(Limit) ->
    RSS = cg_mem_sup:rss(),
    Total = cg_mem_sup:limit(),
    Details = #{rss => RSS, total => Total},
    limit(RSS * 100 div Total, Limit, Details).

%% disk limit
%% 3-th element from disksup:get_disk_data()
-spec disk(string(), number()) ->
    result().
disk(Path, Limit) ->
    Details = #{path => Path},
    limit(element(3, lists:keyfind(Path, 1, disksup:get_disk_data())), Limit, Details).

%% just add 'service' to result
-spec service(binary()) ->
    result().
service(ServiceName) ->
    {passing, ServiceName}.

%%

-spec limit(V, V, details()) ->
    check_run_result().
limit(Value, Limit, Details0) ->
    Details = Details0#{value => Value},
    case Limit of
        undefined            -> {passing  , Details};
        _ when Limit > Value -> {passing  , Details#{limit => Limit}};
        _                    -> {critical , Details#{limit => Limit}}
    end.
