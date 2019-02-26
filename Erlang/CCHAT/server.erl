-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    (catch genserver:start(ServerAtom, [], fun handle/2)).

handle(St, {join, Ch, Client}) ->
  case lists:member(Ch, St) of
    true -> Result = (catch genserver:request(list_to_atom(Ch), {join, Client})),
      case Result of
        joined -> {reply, joined, St};
        failed -> {reply, failed, St}
      end;
    false -> (catch genserver:start(list_to_atom(Ch), [Client], fun channel/2)),
      {reply, joined, [Ch | St]}
  end;

handle(St, kill_channels) ->
  % Iterates (hopefully) through all channels registered to a server and
  % stops them
  lists:foreach(fun(Ch) -> (catch genserver:stop(list_to_atom(Ch))) end, St),
  {reply,ok,[]}.

channel(Clients, {join, Client})->
  case lists:member(Client,Clients) of
    true -> {reply, failed, Clients};
    false -> {reply, joined, [Client | Clients]}
  end;

channel(Clients, {leave, Client})->
  case lists:member(Client,Clients) of
    true -> {reply, ok, lists:delete(Client,Clients)};
    false -> {reply, failed, [Clients]}
  end;

channel(Clients, {message, Channel, Nick, Msg, Client})->
  case lists:member(Client,Clients) of
    true -> spawn(fun()->lists:foreach(
      fun(Pid) ->
        if Pid == Client -> skip;
        true -> (catch genserver:request(Pid, {message_receive, Channel, Nick, Msg}))
        end
      end,
    Clients) end),
    {reply, ok, Clients};
    false -> {reply, failed, Clients}
  end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    (catch genserver:request(ServerAtom, kill_channels)),
    (catch genserver:stop(ServerAtom)).
