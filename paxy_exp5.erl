-module(paxy_exp5).
-export([start/1, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie"],
  ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE}],
  AccRegister = [homer, marge, bart, lisa, maggie],
  PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      RemoteAcceptorsNode = 'paxy-acc@127.0.0.1',
      RemoteProposersNode = 'paxy-pro@127.0.0.1',
      spawn(RemoteAcceptorsNode, fun()-> start_acceptors(AccIds, AccRegister)end),
      spawn(fun() ->
        Begin = erlang:monotonic_time(),
        AccRemoteRegister = [
            {homer, RemoteAcceptorsNode},
            {marge, RemoteAcceptorsNode},
            {bart, RemoteAcceptorsNode},
            {lisa, RemoteAcceptorsNode},
            {maggie, RemoteAcceptorsNode}
        ],
        spawn(RemoteProposersNode, fun() ->
            start_proposers(PropIds, PropInfo, AccRemoteRegister, Sleep, self())
        end),
        wait_proposers(length(PropIds)),
        End = erlang:monotonic_time(),
        Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
        io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
      end)
  end.

start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      register(RegName, acceptor_exp5:start(RegName, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer_exp5:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
  end.

wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

stop() ->
  RemoteAcceptorsNode = 'paxy-acc@127.0.0.1',
  RemoteProposersNode = 'paxy-pro@127.0.0.1',
  stop({homer, RemoteAcceptorsNode}),
  stop({marge, RemoteAcceptorsNode}),
  stop({bart, RemoteAcceptorsNode}),
  stop({lisa, RemoteAcceptorsNode}),
  stop({maggie, RemoteAcceptorsNode}),
  stop({gui, RemoteProposersNode}).

stop({RegisteredName, RemoteNode}) ->
    case rpc:call(RemoteNode, erlang, whereis, [RegisteredName]) of
    undefined ->
      ok;
    Pid ->
      io:format("[Paxy] Stopping: ~w~n", [RegisteredName]),
      Pid ! stop
  end.

 
