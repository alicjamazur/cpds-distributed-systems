-module(acceptor_exp6).
-export([start/2]).
-define(delay, 4000).


start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
    Promised = order:null(),
    Voted = order:null(),
    Value = na,
    % If there is no previous file, create one with initial values.
    % If there is one, just read from the values stored previously.
    case filelib:is_file(Name) of
        false ->
            pers:open(Name),
            pers:store(Name, Promised, Voted, Value, PanelId),
            pers:close(Name),
            acceptor(Name, Promised, Voted, Value, PanelId);
        true ->
            pers:open(Name),
            {PrePromised, PreVoted, PreValue,Pn} = pers:read(Name),
            pers:close(Name),
            io:format("[ACCEPTOR ~w ] RECOVERED WITH CURRENT STATE:~n
            PROMISED = ~w~n
             VOTED = ~w~n
             VALUE = ~w~n~n",
            [Name, PrePromised, PreVoted, PreValue]),
            acceptor(Name, PrePromised, PreVoted, PreValue, Pn)
    end.

acceptor(Name, Promised, Voted, Value, PanelId) ->

  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          T = rand:uniform(?delay),
          timer:send_after(T, Proposer, {promise, Round, Voted, Value}),
          io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                    [Name, Round, Voted, Value]),
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
                     "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          pers:open(Name),
          pers:store(Name, Round, Voted, Value, PanelId),
          pers:close(Name),
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          Proposer ! {sorry, {prepare, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        true ->
          T = rand:uniform(?delay),
          timer:send_after(T, Proposer, {vote, Round}),
          case order:goe(Round , Voted) of
            true ->
              io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                         [Name, Promised, Round, Proposal]),
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]),
                         "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              pers:open(Name),
              pers:store(Name, Promised, Round, Proposal, PanelId),
              pers:close(Name),
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;
        false ->
          Proposer ! {sorry, {accept, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      pers:delete(Name),
      PanelId ! stop,
      ok
  end.