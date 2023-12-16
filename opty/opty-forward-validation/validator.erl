-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init()->
    validator().

validator() ->
    receive
        {validate, Ref, Writes, Client} ->
            case forward_validate(Writes) of
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

forward_validate(Writes) ->
    Result = lists:foldl(
        fun({_, Entry, _}, Acc) when Acc =:= ok ->
                Entry ! {check, self()},
                receive
                    {active_read_conflict, _} ->
                        conflict;
                    {no_active_read_conflict, _} ->
                        ok
                end;
           (_, Acc) ->
                Acc
        end, ok, Writes),
    Result.
