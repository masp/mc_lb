mc_lb
========

> An experiment to make an easily scalable Layer 7 proxy for Minecraft's protocol

Features
--------

- [X] Proxy a single Minecraft 1.16.4 server
- [X] Proxy many servers behind single proxy
- [ ] Proxy many servers behind many proxies
- [ ] Global chat
- [X] Global teleportation

Similar Projects
----------------

- [BungeeCord](https://github.com/SpigotMC/BungeeCord)
- [Waterfall](https://github.com/PaperMC/Waterfall)

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