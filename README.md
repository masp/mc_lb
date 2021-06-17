mc_lb
========

> An experiment to make an infinitely scalable L7 loadbalancer for Minecraft's protocol

Features
--------

- [X] Proxy a single Minecraft 1.16.4 server
- [ ] Proxy many servers behind single proxy
- [ ] Proxy many servers behind many proxies
- [ ] Global chat

Pre-requisites
--------------

- Erlang/OTP 21+ ([download here](https://www.erlang.org/))
- Rebar3 ([download here](https://rebar3.readme.io/docs/getting-started#installing-binary))
- Linux (only testing on Linux, should work without too much effort on Windows or OSX)

Run/Debug
-----

```shell
rebar3 shell
```

Test
----

```shell
rebar3 eunit
```