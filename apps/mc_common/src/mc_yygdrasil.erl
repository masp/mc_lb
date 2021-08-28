-module(mc_yygdrasil).

-export([
    broken_sha1/1,
    user_has_joined/2
]).

-include_lib("eunit/include/eunit.hrl").

-spec broken_sha1(iodata()) -> binary().
broken_sha1(Data) ->
    Bin = crypto:hash(sha, Data),
    S =
        case Bin of
            <<1:1, Rest/bitstring>> ->
                % Calculate 2's complement of the positive part and negate the hex encoding
                Number = binary:decode_unsigned(<<0:1, Rest/bitstring>>),
                V = trunc(math:pow(2, bit_size(Rest))) - Number,
                <<"-", (integer_to_binary(V, 16))/binary>>;
            <<0:1, _/bitstring>> ->
                integer_to_binary(binary:decode_unsigned(Bin), 16)
        end,
    string:lowercase(S).

broken_sha1_test() ->
    ?assertEqual(<<"4ed1f46bbe04bc756bcb17c0c7ce3e4632f06a48">>, broken_sha1(<<"Notch">>)),
    ?assertEqual(<<"-7c9d5b0044c130109a5d7b5fb5c317c02b4e28c1">>, broken_sha1(<<"jeb_">>)),
    ?assertEqual(<<"88e16a1019277b15d58faf0541e11910eb756f6">>, broken_sha1(<<"simon">>)).

-spec user_has_joined(binary(), binary()) -> #{id => binary(), name => binary()}.
user_has_joined(Username, ServerHash) ->
    URL = <<
        "https://sessionserver.mojang.com/session/minecraft/hasJoined?",
        "username=",
        Username/binary,
        "&serverId=",
        ServerHash/binary
    >>,
    Options = [],
    {ok, 200, _RespHeaders, ClientRef} = hackney:get(URL, [], <<>>, Options),
    {ok, Body} = hackney:body(ClientRef),
    #{<<"id">> := Id, <<"name">> := Name} = jsone:decode(Body),
    #{id => uuid:string_to_uuid(Id), name => Name}.
