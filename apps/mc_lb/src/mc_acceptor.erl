-module(mc_acceptor).

-include_lib("public_key/include/public_key.hrl").
-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).

-export([start_loop/1, encode_pubkey/1]).

-record(data, {
    pubkey :: crypto:rsa_public(),
    privkey :: crypto:rsa_private()
}).

-record(request, {
    socket :: mc_socket:socket(),
    verify_token = <<>> :: binary(),
    name = <<>> :: binary()
}).

-spec start_link(gen_tcp:socket()) -> {ok, pid()}.
start_link(LSocket) ->
    Pid = spawn_link(?MODULE, start_loop, [LSocket]),
    {ok, Pid}.

start_loop(LSocket) ->
    Privkey = public_key:generate_key({rsa, 1024, 65537}),
    Pubkey = #'RSAPublicKey'{
        modulus = Privkey#'RSAPrivateKey'.modulus,
        publicExponent = Privkey#'RSAPrivateKey'.publicExponent
    },
    loop(LSocket, #data{pubkey = Pubkey, privkey = Privkey}).

loop(LSocket, Data) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    {ok, McSocket} = mc_socket:wrap(Socket, clientbound, mc_protocol:init()),
    {packet, handshake, #{nextstate := NextState}} = mc_socket:recv(McSocket),
    ok = mc_socket:change_state(McSocket, NextState),
    ok = handle_request(#request{socket = McSocket}, NextState, Data),
    loop(LSocket, Data).

handle_request(Req, login, Data) ->
    handle_login(online_mode(), Req, Data),
    start_child(Req),
    ok;
handle_request(#request{socket = Sock} = Req, status, Data) ->
    case mc_socket:recv(Sock) of
        {packet, status_req, _} ->
            JsonResp = jsone:encode(#{
                version => #{
                    name => <<"1.17.1">>,
                    protocol => 756
                },
                players => #{
                    max => 100,
                    % TODO: Get centralized count of players from somewhere
                    online => 0,
                    sample => []
                },
                description => #{
                    text => <<"It's a server?">>
                }
            }),
            ok = mc_socket:send(Sock, status_resp, #{json_resp => JsonResp}),
            handle_request(Req, status, Data);
        {packet, ping, Packet} ->
            ok = mc_socket:send(Sock, pong, Packet),
            ok = mc_socket:shutdown(Sock);
        closed ->
            ok;
        {error, Reason} ->
            exit(Reason)
    end.

handle_login(offline, #request{socket = Sock} = Req, _Data) ->
    case mc_socket:recv(Sock) of
        {packet, login_start, #{name := PlayerName}} ->
            mc_socket:send(Sock, login_success, #{
                name => PlayerName,
                uuid => uuid:get_v3(<<"OfflinePlayer:">>, PlayerName)
            }),
            mc_socket:change_state(Sock, play),
            Req#request{socket = Sock}
    end;
handle_login(
    online,
    #request{socket = Sock, name = Name, verify_token = Token} = Req,
    #data{pubkey = Pubkey, privkey = Privkey} = Data
) ->
    ServerId = <<>>,
    case mc_socket:recv(Sock) of
        {packet, login_start, #{name := ReqName}} ->
            NewToken = crypto:strong_rand_bytes(4),
            ok = mc_socket:send(Sock, encryption_request, #{
                server_id => ServerId,
                public_key => encode_pubkey(Pubkey),
                verify_token => NewToken
            }),
            handle_login(online, Req#request{name = ReqName, verify_token = NewToken}, Data);
        {packet, encryption_response, #{shared_secret := EncSecret, verify_token := EncToken}} ->
            Token = public_key:decrypt_private(EncToken, Privkey),
            Secret = public_key:decrypt_private(EncSecret, Privkey),
            ServerHash = mc_yygdrasil:broken_sha1([
                ServerId,
                Secret,
                encode_pubkey(Pubkey)
            ]),
            #{id := Id, name := Name} = mc_yygdrasil:user_has_joined(Name, ServerHash),
            ok = mc_socket:set_encryption(Sock, Secret),
            ok = mc_socket:send(Sock, login_success, #{
                name => Name,
                uuid => Id
            }),
            ok = mc_socket:change_state(Sock, play)
    end.

start_child(#request{socket = Sock}) ->
    % Takes control of the TCP socket in start_link
    {ok, _Pid} = supervisor:start_child(mc_players_sup, #{
        id => {mc_player_sup, make_ref()},
        start => {mc_player_sup, start_link, [Sock]},
        restart => temporary
    }),
    unlink(Sock).

online_mode() ->
    case application:get_env(online_mode) of
        undefined -> offline;
        {ok, true} -> online;
        {ok, _} -> offline
    end.

% Encodes the public key using the X509 specification SubjectPublicKeyInfo entry with an RSA public key
% For more info, see https://datatracker.ietf.org/doc/html/rfc5912
encode_pubkey(Pubkey) ->
    public_key:der_encode('SubjectPublicKeyInfo', #'SubjectPublicKeyInfo'{
        algorithm = #'SubjectPublicKeyInfoAlgorithm'{
            algorithm = ?'rsaEncryption',
            % NULL
            parameters = <<5, 0>>
        },
        subjectPublicKey = public_key:der_encode('RSAPublicKey', Pubkey)
    }).
