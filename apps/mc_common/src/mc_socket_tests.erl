-module(mc_socket_tests).

-dialyzer({nowarn_function, socket_setup/1}).

-include_lib("eunit/include/eunit.hrl").

socket_setup(Dir) ->
    fun() ->
        meck:new(gen_tcp, [unstick, stub_all]),
        meck:new(inet, [unstick]),
        {ok, Socket} = mc_socket:wrap(mock_socket, Dir, mc_protocol:init(), #{state => login}),
        Socket
    end.

socket_cleanup(_) ->
    meck:unload(gen_tcp),
    meck:unload(inet).

send_test_() ->
    {foreach, socket_setup(serverbound), fun socket_cleanup/1, [
        fun send/1,
        fun send_crypto/1
    ]}.

send(Socket) ->
    [
        fun() ->
            meck:expect(gen_tcp, send, fun(S, Body) ->
                ?assertEqual(mock_socket, S),
                ?assertEqual(<<5, 0, 3, "abc">>, Body)
            end),
            mc_socket:send(Socket, login_start, #{name => <<"abc">>})
        end,
        ?_assert(meck:validate(gen_tcp))
    ].

send_crypto(Socket) ->
    [
        fun() ->
            Secret = <<0:128>>,
            mc_socket:set_encryption(Socket, Secret),
            Dec = crypto:crypto_init(aes_cfb8, Secret, Secret, [{encrypt, false}]),
            % Expect Login start packet binary received
            meck:expect(gen_tcp, send, fun(S, Body) ->
                ?assertEqual(mock_socket, S),
                ?assertEqual(<<5, 0, 3, "abc">>, crypto:crypto_update(Dec, Body))
            end),
            mc_socket:send(Socket, login_start, #{name => <<"abc">>})
        end,
        ?_assert(meck:validate(gen_tcp))
    ].

recv_test_() ->
    {foreach, socket_setup(clientbound), fun socket_cleanup/1, [
        fun recv/1,
        fun recv_encrypted/1,
        fun recv_passive/1
    ]}.

recv(Socket) ->
    meck:expect(inet, setopts, fun(_Socket, _Opts) ->
        Socket ! {tcp, mock_socket, <<5, 0>>},
        Socket ! {tcp, mock_socket, <<3, "abc">>},
        ok
    end),
    [
        ?_assertEqual({packet, login_start, #{name => <<"abc">>}}, mc_socket:recv(Socket)),
        ?_assert(meck:validate(inet))
    ].

recv_encrypted(Socket) ->
    Secret = <<0:128>>,
    mc_socket:set_encryption(Socket, Secret),
    Enc = crypto:crypto_init(aes_cfb8, Secret, Secret, [{encrypt, true}]),
    meck:expect(inet, setopts, fun(_Socket, _Opts) ->
        Socket ! {tcp, mock_socket, crypto:crypto_update(Enc, <<5, 0, 3, "abc">>)},
        ok
    end),
    [
        ?_assertEqual({packet, login_start, #{name => <<"abc">>}}, mc_socket:recv(Socket)),
        ?_assert(meck:validate(inet))
    ].

recv_passive(Socket) ->
    meck:expect(inet, setopts, fun(_Socket, _Opts) ->
        Socket ! {tcp, mock_socket, <<5, 0>>},
        Socket ! {tcp, mock_socket, <<3, "abc">>},
        ok
    end),

    [
        ?_assertEqual(ok, mc_socket:recv_passive(Socket)),
        fun() ->
            receive
                {Socket, Msg} -> ?assertEqual({packet, login_start, #{name => <<"abc">>}}, Msg)
            end
        end,
        ?_assert(meck:validate(inet))
    ].
