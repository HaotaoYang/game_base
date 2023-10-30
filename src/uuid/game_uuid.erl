%%%-------------------------------------------------------------------
%%% @doc
%%% 创建唯一id
%%% @end
%%%-------------------------------------------------------------------
-module(game_uuid).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    gen_id/1
]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
}).

-define(ETS_TABLE, ?MODULE).
-define(FLAG_TIME, 1672531200000). %% 2023-01-01 00:00:00 (UTC)
-define(MILLI_TIMESTAMP, erlang:system_time(millisecond)).  %% 毫秒时间戳

%% 64bit: 1bit-符号位 41bit-时间 10bit-服务器id 12bit-序列号(可根据实际应用场景调整位数)
-define(GEN_ID(MilliTime, ServerId, Seq), ((MilliTime - ?FLAG_TIME) bsl 22) + (ServerId bsl 12) + Seq).
-define(MAX_SERVER_ID, 1 bsl 10).   % 最大值 1024
-define(MAX_SEQ_ID, 1 bsl 12).      % 最大值 4096

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

gen_id(ServerId) when ServerId < ?MAX_SERVER_ID ->
    MilliTime = ?MILLI_TIMESTAMP,
    case ets:update_counter(?ETS_TABLE, MilliTime, 1, {MilliTime, 0}) of
        Seq when Seq < ?MAX_SEQ_ID ->
            ?GEN_ID(MilliTime, ServerId, Seq);
        _ ->
            timer:sleep(1),
            gen_id(ServerId)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?ETS_TABLE, [named_table, public, set]),
    start_clear_timer(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(clear, State) ->
    MilliTime = ?MILLI_TIMESTAMP,
    ets:foldl(
        fun({Key, _Value}, _Acc) ->
            case Key < MilliTime of
                true -> ets:delete(?ETS_TABLE, Key);
                _ -> skip
            end
        end,
        [],
        ?ETS_TABLE
    ),
    start_clear_timer(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc 启动清除定时器(10s)
start_clear_timer() ->
    erlang:send_after(10 * 1000, self(), clear).
