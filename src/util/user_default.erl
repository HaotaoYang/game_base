-module(user_default).

%% API
-export([
    help/0,
    cmd/1,
    top/0,
    top/1,
    system_info/1,
    process_info/1,
    process_info/2,
    memory/0,
    garbage_collect/0
]).

%%====================================================================
%% API functions
%%====================================================================
help() ->
    shell_default:help(),
    io:format("** user extended commands **~n"),
    io:format("cmd(Str)                     -- 返回shell命令的结果\n"),
    io:format("top()                        -- 输出排名前3的进程信息\n"),
    io:format("top(N)                       -- 输出排名前几的进程信息\n"),
    io:format("system_info(process_limit)   -- 返回当前节点可存在的最大进程数，这个限制数可以在节点启动的时候由参数 +P 设置\n"),
    io:format("system_info(process_count)   -- 返回当前节点的进程数，结果跟length(erlang:processes())一样\n"),
    io:format("system_info(wordsize)        -- 当前系统1word代表的字节数\n"),
    io:format("process_info(Pid)            -- 返回一个进程的信息\n"),
    io:format("process_info(Pid, ItemSpec)  -- 获取进程的相关信息\n"),
    io:format("ets:all()                    -- 获取所有ets表\n"),
    io:format("ets:info(T)                  -- 返回一个ets表信息\n"),
    io:format("ets:info(T, Item)            -- 获取ets表相关信息\n"),
    io:format("memory()                     -- 查看当前节点内存信息\n"),
    io:format("garbage_collect()            -- 回收所有进程内存\n"),
    io:format("eprof_start(AppList)         -- 监控指定的app列表，请谨慎使用\n"),
    io:format("eprof_stop()                 -- 查看监控的结果信息\n"),
    io:format("fprof_start(AppList)         -- 监控指定的app列表并写入文件，请谨慎使用\n"),
    io:format("fprof_start_pid(Pid)         -- 监控单个进程并写入文件\n"),
    io:format("fprof_stop()                 -- 查看监控的结果信息并写入文件\n"),
    true.

cmd(Str) ->
    RetMsg = os:cmd(Str),
    MsgList = string:tokens(RetMsg, "\n"),
    lists:foreach(fun(Msg) -> io:format("~p~n", [Msg]) end, MsgList).

%% 输出排名前几的进程信息
top() -> top(3).
top(N) ->
    #{
        reductions => recon:proc_count(reductions, N),
        memory => recon:proc_count(memory, N),
        message_queue_len => recon:proc_count(message_queue_len, N)
    }.

system_info(Item) -> erlang:system_info(Item).

process_info(Pid) -> erlang:process_info(Pid).

process_info(Pid, ItemSpec) -> erlang:process_info(Pid, ItemSpec).

memory() ->
    [
        case Key of
            total -> io:format("total:           ~18.16fG  当前分配给进程(processes)和系统(system)的内存总量~n", [Value / math:pow(1024, 3)]);
            processes -> io:format("processes:       ~18.16fG  当前分配给Erlang进程的内存总量~n", [Value / math:pow(1024, 3)]);
            processes_used -> io:format("processes_used:  ~18.16fG  当前已被Erlang进程使用的内存总量(进程内存的一部分)~n", [Value / math:pow(1024, 3)]);
            system -> io:format("system:          ~18.16fG  当前分配给Erlang系统的内存总量~n", [Value / math:pow(1024, 3)]);
            atom -> io:format("atom:            ~18.16fG  当前分配给原子的内存总量(系统内存的一部分)~n", [Value / math:pow(1024, 3)]);
            atom_used -> io:format("atom_used:       ~18.16fG  当前已被原子使用的内存总量(系统内存的一部分)~n", [Value / math:pow(1024, 3)]);
            binary -> io:format("binary:          ~18.16fG  当前分配给二进制数据的内存总量(系统内存的一部分)~n", [Value / math:pow(1024, 3)]);
            code -> io:format("code:            ~18.16fG  当前代码数据所占用的内存总量(系统内存的一部分)~n", [Value / math:pow(1024, 3)]);
            ets -> io:format("ets:             ~18.16fG  当前分配给ets表的内存总量(系统内存的一部分)~n", [Value / math:pow(1024, 3)]);
            _ -> skip
        end || {Key, Value} <- erlang:memory()
    ].

garbage_collect() ->
    [erlang:garbage_collect(Pid) || Pid <- erlang:processes()].

eprof_start(AppList) ->
    eprof:start(),
    case lists:keyfind(running, 1, application:info()) of
        {_, Apps} ->
            case get_procs(AppList, Apps) of
                [] ->
                    {error, no_procs_found};
                Procs ->
                    eprof:start_profiling(Procs)
            end;
        _ ->
            {error, no_app_info}
    end.

eprof_stop() ->
    eprof:stop_profiling(),
    eprof:analyze().

fprof_start(AppList) ->
    fprof_start(AppList, 0).

fprof_start(AppList, Duration) ->
    case lists:keyfind(running, 1, application:info()) of
        {_, Apps} ->
            case get_procs(AppList, Apps) of
                [] ->
                    {error, no_procs_found};
                Procs ->
                    fprof:trace([start, {file, "./log/fprof.trace"}, {procs, Procs}]),
                    io:format("Profiling started~n"),
                    case Duration > 0 of
                        true ->
                            timer:sleep(Duration * 1000),
                            fprof:trace([stop]),
                            fprof:stop();
                        _ ->
                            ok
                    end
            end;
        _ ->
            {error, no_app_info}
    end.

fprof_start_pid(Pid) ->
    fprof:trace([start, {file, "./log/fprof.trace"}, {procs, Pid}]),
    io:format("Profiling started~n").

fprof_stop() ->
    fprof:trace([stop]),
    fprof:profile({file, "./log/fprof.trace"}),
    fprof:analyse([totals, no_details, {sort, own}, no_callers, {dest, "./log/fprof.analysis"}]),
    fprof:stop(),
    format_fprof_analyze().

%%====================================================================
%% Internal functions
%%====================================================================

get_procs(AppList, Apps) ->
    io:format("Searching for processes to profile...~n", []),
    Procs = lists:flatten(
        lists:foldl(
            fun
                ({App, Leader}, Acc) when is_pid(Leader) ->
                    case lists:member(App, AppList) of
                        true -> [get_procs2(Leader) | Acc];
                        _ -> Acc
                    end;
                (_, Acc) -> Acc
            end,
            [],
            Apps
        )
    ),
    io:format("Found ~p processes~n", [length(Procs)]),
    Procs.

get_procs2(Leader) ->
    lists:filter(
        fun(Pid) ->
            case erlang:process_info(Pid, group_leader) of
                {_, Leader} -> true;
                _ -> false
            end
        end,
        processes()
    ).

format_fprof_analyze() ->
    case file:consult("./log/fprof.analysis") of
        {ok, [_, [{totals, _, _, TotalOWN}] | Rest]} ->
            OWNs =
                lists:flatmap(
                    fun({MFA, _, _, OWN}) ->
                        Percent = OWN * 100 / TotalOWN,
                        case round(Percent) of
                            0 -> [];
                            _ -> [{mfa_to_list(MFA), Percent}]
                        end
                    end,
                    Rest
                ),
            ACCs = collect_accs(Rest),
            MaxACC = find_max(ACCs),
            MaxOWN = find_max(OWNs),
            io:format("=== Sorted by OWN:~n"),
            lists:foreach(
                fun({MFA, Per}) ->
                    L = length(MFA),
                    S = lists:duplicate(MaxOWN - L + 2, $ ),
                    io:format("~s~s~.2f%~n", [MFA, S, Per])
                end,
                lists:reverse(lists:keysort(2, OWNs))
            ),
            io:format("~n=== Sorted by ACC:~n"),
            lists:foreach(
                fun({MFA, Per}) ->
                    L = length(MFA),
                    S = lists:duplicate(MaxACC - L + 2, $ ),
                    io:format("~s~s~.2f%~n", [MFA, S, Per])
                end,
                lists:reverse(lists:keysort(2, ACCs))
            );
        Err ->
            Err
    end.

mfa_to_list({M, F, A}) ->
    atom_to_list(M) ++ ":" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A);
mfa_to_list(F) when is_atom(F) ->
    atom_to_list(F).

find_max(List) ->
    find_max(List, 0).

find_max([{V, _}|Tail], Acc) ->
    find_max(Tail, lists:max([length(V), Acc]));
find_max([], Acc) ->
    Acc.

collect_accs(List) ->
    List1 = lists:filter(
        fun({{sys, _, _}, _, _, _}) ->
            false;
            ({suspend, _, _, _}) ->
                false;
            ({{gen_fsm, _, _}, _, _, _}) ->
                false;
            ({{gen, _, _}, _, _, _}) ->
                false;
            ({{gen_server, _, _}, _, _, _}) ->
                false;
            ({{proc_lib, _, _}, _, _, _}) ->
                false;
            (_) ->
                true
        end,
        List
    ),
    calculate(List1).

calculate(List1) ->
    TotalACC = lists:sum([A || {_, _, A, _} <- List1]),
    List2 = lists:foldl(
        fun({MFA, _, ACC, _}, NewList) ->
            Percent = ACC * 100 / TotalACC,
            case round(Percent) of
                0 -> NewList;
                _ -> [{mfa_to_list(MFA), Percent} | NewList]
            end
        end,
        [],
        List1
    ),
    lists:reverse(List2).
