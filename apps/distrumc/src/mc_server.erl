-module(mc_server).

-behaviour(gen_server).

-include("mc.hrl").

-export([start_link/0, join/1, spawn/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(state, {
    next_entity_id = 1 :: integer()
}).

-define(SPAWN_POSITION, {0, 64, 0}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(players, [named_table, {keypos, 2}, protected]),
    {ok, #state{}}.

handle_call({join, Name}, {Pid, _Ref}, #state{next_entity_id = NextEid} = State) ->
    Uuid = create_player(Pid, NextEid, Name),
    _ = erlang:monitor(process, Pid),
    {reply, {ok, Uuid}, State#state{next_entity_id = NextEid + 1}};
handle_call(spawn, {Pid, _Ref}, #state{next_entity_id = NextEid} = State) ->
    ExistPlayers = [Player || Player = #player{pid = P} <- ets:tab2list(players), P =/= Pid],
    spawn_player(Pid),
    {reply, {ok, ExistPlayers}, State#state{next_entity_id = NextEid + 1}};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    remove_player(Pid),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

join(Name) ->
    gen_server:call(?MODULE, {join, Name}).

spawn() ->
    gen_server:call(?MODULE, spawn).

create_player(NewPid, Eid, Name) ->
    Uuid = uuid:get_v3(<<"OfflinePlayer:">>, Name),
    Pos = ?SPAWN_POSITION,
    Rot = {0, 0},
    true = ets:insert_new(players, #player{
        pid = NewPid,
        uuid = Uuid,
        eid = Eid,
        name = Name,
        pos = Pos,
        rot = Rot
    }),
    Uuid.

remove_player(Pid) ->
    case ets:lookup(players, Pid) of
        [Player] ->
            broadcast_message(Pid, {remove_player, Player}),
            ets:delete(players, Pid);
        [] ->
            true
    end.

spawn_player(NewPlayerPid) ->
    [NewPlayer] = ets:lookup(players, NewPlayerPid),
    broadcast_message(NewPlayerPid, {spawn_player, NewPlayer}).

broadcast_message(From, Msg) ->
    io:format("broadcasting ~p~n", [Msg]),
    broadcast_message(From, Msg, ets:first(players)).

broadcast_message(_From, _Msg, '$end_of_table') ->
    ok;
broadcast_message(From, Msg, Player) ->
    io:format("broadcasting from ~p to ~p with ~p~n", [From, Player, Msg]),
    Next = send_msg(From, Player, Msg),
    broadcast_message(From, Msg, Next).

send_msg(From, From, _Msg) ->
    ets:next(players, From);
send_msg(_From, To, Msg) ->
    Next = ets:next(players, To),
    try
        gen_statem:call(To, Msg)
    catch
        exit:{noproc, _} ->
            remove_player(To)
    end,
    Next.
