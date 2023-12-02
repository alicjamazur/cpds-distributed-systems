-module(paxyEx5).
-export([start/1, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  AcceptorNode = 'paxy-acc@Loriens-MacBook-Air', % Node where acceptors will run
  ProposerNode = 'paxy-pro@Loriens-MacBook-Air', % Node where proposers will run
  AcceptorNames = ["Homer", "Marge", "Bart", "Lisa", "Maggie"],
  AccRegister = [homer, marge, bart, lisa, maggie],
  DistributedAccRegister = [{homer,AcceptorNode}, {marge,AcceptorNode}, {bart,AcceptorNode}, {lisa,AcceptorNode}, {maggie,AcceptorNode}],
  ProposerNames = [{"Fry", ?RED}, {"Bender", ?GREEN}, {"Leela", ?BLUE}],
  PropInfo = [{fry, ?RED}, {bender, ?GREEN}, {leela, ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      spawn(AcceptorNode, fun()-> start_acceptors(AccIds, AccRegister)end),
      spawn(fun() -> 
        Begin = erlang:monotonic_time(),
        spawn(ProposerNode,fun() -> start_proposers(PropIds, PropInfo, DistributedAccRegister, Sleep, self()) end),
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
      register(RegName, acceptorEx5:start(RegName, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposerEx5:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),	
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
  AcceptorNode = 'paxy-acc@Loriens-MacBook-Air',
  stop({homer, AcceptorNode}),
  stop({marge, AcceptorNode}),
  stop({bart, AcceptorNode}),
  stop({lisa, AcceptorNode}),
  stop({maggie, AcceptorNode}),
  stop(gui), % Assuming GUI is local
  ok.
stop(Name) ->
  case Name of
    {RegName, Node} when is_atom(RegName), is_atom(Node) ->
      rpc:call(Node, erlang, whereis, [RegName]),
      rpc:call(Node, erlang, send, [RegName, stop]);
    _ ->
      case whereis(Name) of
        undefined ->
          ok;
        Pid ->
          Pid ! stop
      end
  end.

 
