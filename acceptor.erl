-module(acceptor).
-export([start/2]).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
  Promised = order:null(), 
  Voted = order:null(),
  Value = na,
  acceptor(Name, Promised, Voted, Value, PanelId).
%We define the acceptor function right here.
acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      %We need to compare the two tuples. 
      case order:gr(Round, Promised) of
        %If the round that we get is greater, we need inform the proposer and update values.
        true ->
          %We send a message to the proposer with the round which we are 
          %promising, the last voted value, and the value (color)
          Proposer ! {promise, Round, Voted, Value},               
      io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
        %Dsiplay values in terminal
                 [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          %We send sorry message with the value that we promised
          Proposer ! {sorry, {prepare, Promised}},
          %We call the acceptor function again and restart the process, with
          %the same promised value that we had before
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    %In case we receive an accept message from Proposer:
    % (This happens when the proposer receives enough messages with promises from acceptos)
    {accept, Proposer, Round, Proposal} ->
      %We check if the round is greater than promised
      case order:goe(Round, Promised) of
        true ->
          %We also check it the round is greater than the last previous voted.
          Proposer ! {vote, Round},
          case order:goe(Round , Voted) of
            true ->
              %Log in terminal the new 
      io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                 [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), 
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              %We call the acceptor function again, with new values for the 
              %last voted rounda and the color.
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              %If the round is not the same or greater than the last voted,
              %we just call the acceptor with the old values.
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;                            
        false ->
          Proposer ! {sorry, {accept, Promised}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.
