-module(acceptorEx3).
-export([start/2]).
-define(drop, 6).



start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
  Promised = order:null(),
  Voted = order:null(),
  Value = na,
  acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->

  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          P = rand:uniform(10),
          if P =< ?drop ->
           io:format("Promise message dropped by acceptor [~w]~n",[Name]);
          true ->
          Proposer ! {promise, Round, Voted, Value}
          end,
          io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                    [Name, Round, Voted, Value]),
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          Proposer ! {sorry, {prepare, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        true ->
          P = rand:uniform(10),
          if P =< ?drop ->
           io:format("Vote message dropped by acceptor [~w]~n",[Name]);
          true ->
          Proposer ! {vote, Round}
          end,
          case order:goe(Round , Voted) of
            true ->
              io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                         [Name, Promised, Round, Proposal]),
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]),
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;
        false ->
          Proposer ! {sorry, {accept, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.
