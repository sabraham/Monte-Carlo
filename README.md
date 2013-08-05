# montecarlo

Palaces are for royalty. We're just common people with a bank account.

## Starting the server

To run the poker server

```
lein run
```

this will start a server that is listening for TCP connections on port 10000.

## Playing clients

Clients communicate with the server by sending (and receiving) JSON. Currently the commands `new_room`, `join_room`, `play`, `hand`, and `whoami` are supported. The client attaches the command to the `type` key of the request.

### `new_room`

Arg | Type | Doc|
--- | --- | ---
name | string | Name of the room to create
n | int | Number of players to play game

Example:
```
{"type": "new_room", "name": "hogwarts", "n": 3}
*{"status":0,"msg":"OK"}*
```

### `join_room`

Arg | Type | Doc|
--- | --- | ---
name | string | Name of the room to join

Example:
```
{"type": "join_room", "name": "hogwarts"}
*{"status":0,"msg":"OK"}*
```

When `n` players join the room, the game starts automatically, sending all connected clients their cards and the current board state.

Example response:
```
{"card":{"suit":"hearts","rank":8},"room":"hogwarts"}
{"card":{"suit":"clubs","rank":9},"room":"hogwarts"}
{"players":[{"id":"G__13215","stack":95},{"id":"G__13216","stack":90},{"id":"G__13214","stack":100}],"time":0,"play-order":["G__13214","G__13215","G__13216"],"remaining-players":["G__13215","G__13216","G__13214"],"pots":[],"bets":[{"bet":5,"players":["G__13215","G__13216"],"original-players":["G__13215","G__13216"],"n":2},{"bet":5,"players":["G__13216"],"original-players":["G__13216"],"n":1}],"community-cards":[]}
```

### `play`

Arg | Type | Doc|
--- | --- | ---
name | string | Name of the room to send action
amt | int | Action to play

`amt` |  Action
--- |  ---
negative | `fold`
0 | `call`
positive | `raise`

Example of calling:
```
{"type": "play", "name": "default", "amt": 0}
````

### `hand?`
Querying current hand

Example:
```
{"type": "hand?", "name": "hogwarts", "amt": 0}
*{"hand":[{"suit":"hearts","rank":8},{"suit":"clubs","rank":9}]}*
```

### `whoami?`
Querying which player you are

Example:
```
{"type": "whoami?"}
*{"id": "G__13193"}*
```