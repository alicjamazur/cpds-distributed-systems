-module(proposer).
-export([start/6]).

-define(timeout, 2000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId, Main) end).

init(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  timer:sleep(Sleep),
  Begin = erlang:monotonic_time(),
  Round = order:first(Name),
  {Decision, LastRound} = round(Name, ?backoff, Round, Proposal, Acceptors, PanelId),
  End = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
  io:format("[Proposer ~w] DECIDED ~w in round ~w after ~w ms~n", 
             [Name, Decision, LastRound, Elapsed]),
  Main ! done,
  PanelId ! stop.

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
  %We start this function by printing out the current round and proposal.
  io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n", 
             [Name, Round, Proposal]),
  % Update gui
  PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Proposal},
  %If the ballot function returns an "ok" message, we update the 
  %round and value of the proposer. If not we abort and we perform
  %a timeout which increases every time we abort.
  case ballot(Name, Round, Proposal, Acceptors, PanelId) of
    {ok, Value} ->
      {Value, Round};
    abort ->
      timer:sleep(rand:uniform(Backoff)),
      Next = order:inc(Round),
      %We increase the round if we did not receive an ok message.
      round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
  end.
%The vallor function is what manages the prepare and accept
%requests. Basically the body of the paxos algorithm.
ballot(Name, Round, Proposal, Acceptors, PanelId) ->
  %Send the prepare messages to all acceptors. First phase.
  prepare(Round, Acceptors),
  %The Quorum is the majority of votes needed to accept a value.
  Quorum = (length(Acceptors) div 2) + 1,
  MaxVoted = order:null(),
  %WIn the case of the collect function giving an accepted
  %message
  case collect(Quorum, Round, MaxVoted,Proposal ) of
    % (I am not sure if here it should be value or proposal)
    {accepted, Value} ->
      io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", 
                 [Name, Round, Value, Proposal]),
      % update gui
      PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
      %Since now we have changed the value of the proposal to 
      %the value given by the collect function. We send the accept
      %messages informing all acceptors.
      accept(Round, Value, Acceptors),
      %When we reach the quorum for that round, we either arrive to a decision or abort.
      case vote(Quorum, Round) of
        ok ->
          {ok, Value};
        abort ->
          abort
      end;
    abort ->
      abort
  end.

%If there are no votes to collect then we indicate that we have received enough promises to proceed.
collect(0, _, _, Proposal) ->
  {accepted, Proposal};
collect(N, Round, MaxVoted, Proposal) ->
  %I think in the case of receiving empty proposals we just keep collecting them.
  receive 
    {promise, Round, _, na} ->
      collect(N-1, Round, MaxVoted, Proposal);
    {promise, Round, Voted, Value} ->
      %We compare the voted value with the previous maximum voted value
      case order:gr(Voted, MaxVoted) of
        %If the voted value is greater than the maximum voted, we continue collecting with the updated voted number and the value.
        true ->
          collect(N-1,Round , Voted, Value);
        %If the voted is below the maximum voted then we continue with the maximum previous vote and the original proposal.
        false ->
          collect(N-1, Round, MaxVoted, Proposal)
      end;
    {promise, _, _,  _} ->
      collect(N, Round, MaxVoted, Proposal);
    {sorry, {prepare, Round}} ->
      collect(N-1, Round, MaxVoted, Proposal);
    {sorry, _} ->
      collect(N, Round, MaxVoted, Proposal)
  after ?timeout ->
    abort
  end.

%When no more votes are received it probably means we have reached a consensus.
vote(0, _) ->
  ok;
vote(N, Round) ->
  receive
    %When we receive a vote we recursively call the function and then we decrease the number of votes.
    {vote, Round} ->
      vote(N-1, Round);
    %This receive message catches the round numbers that are not from this round (previous rounds) and ignores them.
    {vote, _} ->
      vote(N, Round);

    {sorry, {accept, Round}} ->
      vote(N-1, Round);
    {sorry, _} ->
      vote(N, Round)
  after ?timeout ->
    abort
  end.

prepare(Round, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {prepare, self(), Round}) 
  end,
  lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
  Fun = fun(Acceptor) -> 
    send(Acceptor, {accept, self(), Round, Proposal}) 
  end,
  lists:foreach(Fun, Acceptors).

send(Name, Message) ->
  Name ! Message.
