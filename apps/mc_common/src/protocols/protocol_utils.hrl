-define(PACKET(ID, State, Dir, Name), {name, {ID, State, Dir}} -> Name; {id, {Name, Dir}} -> ID).