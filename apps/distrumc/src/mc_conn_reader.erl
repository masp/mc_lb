-module(mc_conn_reader).

-export([start_link/2, init/1]).

% 5 is the max size of a varint
-define(MAX_LEN_HEADER_SIZE, 5).
-define(TIMEOUT, 32000).

-record(state, {info, parent, socket}).

start_link(ParentPid, Socket) ->
    {ok, {IP, Port}} = inet:peername(Socket),
    spawn_link(?MODULE, init, [#state{info = {IP, Port}, parent = ParentPid, socket = Socket}]).

init(State) ->
    do_recv(State, <<>>).

do_recv(#state{socket = Sock} = State, Buffer) ->
    case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
        {ok, Bin} ->
            Remaining = try_decode_packet(State, <<Buffer/binary, Bin/binary>>),
            do_recv(State, Remaining);
        {error, closed} ->
            {IP, Port} = State#state.info,
            io:format("player disconnected: ~p:~p~n", [IP, Port]),
            exit(disconnected);
        {error, Reason} ->
            exit(Reason)
    end.

try_decode_packet(State, Buffer) ->
    case mc_varint:decode(Buffer) of
        {Len, Body} ->
            if
                byte_size(Body) >= Len ->
                    <<Packet:Len/binary-unit:8, Remaining/binary>> = Body,
                    handle_decoded_packet(State#state.parent, Packet),
                    try_decode_packet(State, Remaining);
                true ->
                    do_recv(State, Buffer)
            end;
        needmore ->
            do_recv(State, Buffer)
    end.

handle_decoded_packet(ParentPid, Packet) ->
    {ID, Body} = mc_varint:decode(Packet),
    mc_conn:receive_packet(ParentPid, ID, Body).
