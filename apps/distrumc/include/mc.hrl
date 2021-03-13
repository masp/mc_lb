-record(player, {
    pid :: pid(),
    eid :: integer(),
    uuid :: <<_:128>>,
    name :: binary(),
    pos :: {float(), float(), float()},
    rot :: {float(), float()}
}).
