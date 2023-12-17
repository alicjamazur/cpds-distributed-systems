-module(server).
-export([start/1]).

start(N) ->
    spawn(fun() -> init(N) end).

init(N) ->
    Store = store:new(N),
    Validator = validator:start(),
    server(Validator, Store).
    
server(Validator, Store) ->
    receive 
        {open, Client} ->
            io:format("[Server][~w] Sending `transaction` request to the Client ~w~n", [node(), Client]),
            Client ! {transaction, Validator, Store},
            server(Validator, Store);
        stop ->
            Validator ! stop,
            store:stop(Store)
    end.
