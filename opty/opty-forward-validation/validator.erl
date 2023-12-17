-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init()->
    validator().

validator() ->
    receive
        {validate, Ref, Writes, Client} ->
            send_write_checks(Writes),
            case check_writes(length(Writes)) of
                ok ->
                    update(Writes),
                    Client ! {Ref, ok};
                conflict ->
                    Client ! {Ref, abort}
            end,
            validator();
        stop ->
            ok;
        _Old ->
            validator()
    end.
    
update(Writes) ->
    lists:foreach(fun({_, Entry, Value}) -> 
                  Entry ! {write, Value}
                  end, 
                  Writes).

send_write_checks(Writes) ->
    Self = self(),
    lists:foreach(fun({_, Entry, _}) ->
                  Entry ! {check, Self}
                  end,
                  Writes).

check_writes(0) ->
    ok;
check_writes(N) ->
    receive
        no_active_read_conflict ->
            check_writes(N-1);
        active_read_conflict ->
            conflict
    end.
