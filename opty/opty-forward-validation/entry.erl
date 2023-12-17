-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    ActiveReaders = [],
    entry(Value, make_ref(), ActiveReaders).

entry(Value, Time, ActiveReaders) ->
    receive
        {read, Ref, From} ->
            From ! {Ref, self(), Value, Time},
            NewActiveReaders = [{From, Ref} | ActiveReaders],
            entry(Value, Time, NewActiveReaders);
        {done, From} ->
            NewActiveReaders = lists:filter(fun({Pid, _}) -> Pid =/= From end, ActiveReaders),
            entry(Value, Time, NewActiveReaders);
        {write, New} ->
            entry(New, make_ref(), ActiveReaders);
        {check, ValidatorPid} ->
            case length(ActiveReaders) of
                0 ->
                    ValidatorPid ! no_active_read_conflict;
                _ ->
                    ValidatorPid ! active_read_conflict
            end,
            entry(Value, Time, ActiveReaders);
        stop ->
            ok
    end.
