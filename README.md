distrumc
========

> A fun experiment to make a serverless Minecraft server

Standard Minecraft servers only support running on one physical machine. The only way to add more players with one server is to make the server bigger which has a limit. If you can scale a Minecraft server across machines instead, you can easily add machines as you have more players without needing to buy expensive machines. `distrumc` is an experiment to make Minecraft be able to run across many machines.

Goals
-----

- Learn about horizontal scaling possibilities for Minecraft
- Easy to operate
- Simple architecture
- Have fun

Non-goals
---------

- Be a feature complete Minecraft server
- Be used in production

Current Tasks
-------------

- [ ] Basic Minecraft 1.16.4 server on only one machine! (in progress)
- [ ] Scaling player movements
- [ ] Scaling chunk modifications
- [ ] Scaling tick events like redstone and water flowing

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

Similar Projects
----------------

- [MineCase](https://github.com/dotnetGame/MineCase) - A distributed Minecraft server based on Microsoft Orleans
- [Opencraft/AtLarge Research](https://github.com/atlarge-research/opencraft-opencraft) - A research team studying the scalability challenges in an interactive environment like Minecraft, focusing on aspects like variable consistency guarantees
- [Manycraft](https://ieeexplore.ieee.org/document/6820616) - Abstract architecture for a distributed Minecraft server
