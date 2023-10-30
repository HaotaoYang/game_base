-module(user_default).

%% API
-export([
    help/0,
    cmd/1,
    top/0,
    top/1
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

%%====================================================================
%% Internal functions
%%====================================================================

