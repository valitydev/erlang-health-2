%%%
%%% Стандартные проверки лимитов
%%% Логика работы следующая:
%%%  - если чекер запускается, то нужно добавить запись в результат;
%%%  - если есть лимит в опциях, то нужно проверить его и в случае превышения отдать 503
%%%
-module(erl_health).

%% API
-export([check/1]).
-export([check/2]).

-export([cpu      /1]).
-export([load     /1]).
-export([memory   /1]).
-export([cg_memory/1]).
-export([disk     /2]).
-export([service  /1]).

-export_type([code   /0]).
-export_type([message/0]).
-export_type([result /0]).
-export_type([checker/0]).

%%
%% API
%%
-type code() :: pos_integer(). % FIXME
-type message() :: iolist().

-type result() :: {ok, map()} | {error, code(), message()}.
-type checker() :: {module(), atom(), list()} | fun(() -> result()).

-spec check([checker()]) ->
    result().
check(Checkers) ->
    check(Checkers, #{}).

-spec check([checker()], map()) ->
    result().
check([], Resp) ->
    {ok, Resp};
check([Checker|OtherChecks], Resp) ->
    case call_checker(Checker) of
        {ok, AdditionalResp} ->
            check(OtherChecks, maps:merge(Resp, AdditionalResp));
        Error = {error, _, _} ->
            Error
    end.

-spec call_checker(checker()) ->
    result().
call_checker({M, F, A}) ->
    erlang:apply(M, F, A);
call_checker(Checker) ->
    Checker().

%%

%% cpu utilization limit
%% cpu_sup:util()
-spec cpu(number()) ->
    result().
cpu(Limit) ->
    limit(cpu, cpu_sup:util(), Limit).

%% load limit
%% cpu_sup:avg1()
-spec load(number()) ->
    result().
load(Limit) ->
    limit(load, cpu_sup:avg1(), Limit).

%% memory limit
%% memsup:get_system_memory_data(), (total - free) / total
-spec memory(number()) ->
    result().
memory(Limit) ->
    MemStat = maps:from_list(memsup:get_system_memory_data()),
    Total = maps:get(total_memory, MemStat),
    limit(memory, (Total - maps:get(free_memory, MemStat)) * 100 div Total, Limit).

%% cgroups memory limit
%% /sys/fs/cgroups memory.usage_in_bytes / memory.limit_in_bytes
-spec cg_memory(number()) ->
    result().
cg_memory(Limit) ->
    limit(cg_memory, cg_mem_sup:usage() * 100 div cg_mem_sup:limit(), Limit).

%% disk limit
%% 3-th element from disksup:get_disk_data()
-spec disk(string(), number()) ->
    result().
disk(Path, Limit) ->
    limit(disk, element(3, lists:keyfind(Path, 1, disksup:get_disk_data())), Limit).

%% just add 'service' to result
-spec service(binary()) ->
    result().
service(ServiceName) ->
    {ok, #{service => ServiceName}}.

%%

-spec limit(atom(), V, V) ->
    result().
limit(Key, Value, Limit) ->
    R = case Limit of
            undefined            -> ok;
            _ when Limit > Value -> ok;
            _                    -> error
        end,
    case R of
        ok    -> {ok, #{Key => Value}};
        error -> {error, 503, atom_to_list(Key) ++ " limit reached"}
    end.
