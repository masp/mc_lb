-module(mc_acceptor).

-export([start_link/1, loop/1]).

start_link(LSocket) ->
    Pid = spawn_link(?MODULE, loop, [LSocket]),
    {ok, Pid}.

loop(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    {ok, {IP, _}} = inet:peername(Socket),
    {ok, ConnPid} = supervisor:start_child(mc_conn_sup, #{
        id => {mc_conn, IP},
        start => {mc_conn, start_link, [Socket]},
        restart => temporary
    }),
    % safe from race condition because {active, false} is specified
    ok = gen_tcp:controlling_process(Socket, ConnPid),
    loop(LSocket).
