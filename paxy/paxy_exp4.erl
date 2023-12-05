-module(paxy_1).
-export([start/1, stop/0, stop/1]).

-define(RED, {255, 0, 0}).
-define(BLUE, {0, 0, 255}).
-define(GREEN, {0, 255, 0}).
-define(YELLOW, {255, 255, 0}).
-define(PURPLE, {128, 0, 128}).
-define(ORANGE, {255, 165, 0}).
-define(CYAN, {0, 255, 255}).
-define(MAGENTA, {255, 0, 255}).
-define(BROWN, {165, 42, 42}). % New color
-define(GOLD, {255, 215, 0}).  % New color

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
    AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie"],
    AccRegister = [homer, marge, bart, lisa, maggie],
    ProposerNames = [
        {"Fry", ?RED},
        {"Bender", ?GREEN},
        {"Leela", ?BLUE},
        {"Amy", ?PURPLE},
        {"Farnsworth", ?ORANGE},
        {"Zoidberg", ?YELLOW},
        {"Hermes", ?CYAN},
        {"Nibbler", ?MAGENTA},
        {"Kif", ?BROWN},
        {"Scruffy", ?GOLD}
    ],
    PropInfo = [
        {fry, ?RED},
        {bender, ?GREEN},
        {leela, ?BLUE},
        {amy, ?PURPLE},
        {farnsworth, ?ORANGE},
        {zoidberg, ?YELLOW},
        {hermes, ?CYAN},
        {nibbler, ?MAGENTA},
        {kif, ?BROWN},
        {scruffy, ?GOLD}
    ],
    register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
    gui ! {reqState, self()},
    receive
        {reqState, State} ->
            {AccIds, PropIds} = State,
            start_acceptors(AccIds, AccRegister),
            spawn(fun() ->
                Begin = erlang:monotonic_time(),
                start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
                wait_proposers(length(PropIds)),
                End = erlang:monotonic_time(),
                Elapsed = erlang:convert_time_unit(End - Begin, native, millisecond),
                io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
            end)
    end.

start_acceptors(AccIds, AccReg) ->
    case AccIds of
        [] ->
            ok;
        [AccId | Rest] ->
            [RegName | RegNameRest] = AccReg,
            register(RegName, acceptor:start(RegName, AccId)),
            start_acceptors(Rest, RegNameRest)
    end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
    case PropIds of
        [] ->
            ok;
        [PropId | Rest] ->
            [{RegName, Colour} | RestInfo] = PropInfo,
            [FirstSleep | RestSleep] = Sleep,
            proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),
            start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
    end.

wait_proposers(0) ->
    ok;
wait_proposers(N) ->
    receive
        done ->
            wait_proposers(N - 1)
    end.

stop() ->
    stop(homer),
    stop(marge),
    stop(bart),
    stop(lisa),
    stop(maggie),
    stop(gui).

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.
