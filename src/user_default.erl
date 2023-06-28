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
    io:format("cmd(Str)   -- 返回shell命令的结果\n"),
    io:format("top()      -- 输出排名前3的进程信息\n"),
    io:format("top(N)     -- 输出排名前几的进程信息\n"),
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

%%====================================================================
%% Internal functions
%%====================================================================


