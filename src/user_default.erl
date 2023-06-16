-module(user_default).

%% API
-export([
    help/0,
    top/0,
    top/1
]).

-import(io, [format/1, format/2]).

%%====================================================================
%% API functions
%%====================================================================
help() ->
    shell_default:help(),
    format("** user extended commands **~n"),
    format("top()      -- 输出排名前3的进程信息\n"),
    format("top(N)     -- 输出排名前几的进程信息\n"),
    true.

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


