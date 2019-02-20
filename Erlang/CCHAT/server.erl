-module(server).
-export([start/1,stop/1]).
-record(server, {server}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, [], fun handle/2).
%
handle(S, {join, Ch, Client}) ->
  case lists:member(Ch, S) of
    true -> Result = genserver:request(list_to_atom(Ch), {join, Client}),
      case Result of
        joined -> {reply, joined, S};
        failed -> {reply, failedToJoin, S}
      end;
    false -> genserver:start(list_to_atom(Ch), S, channel),
      {reply, join, [Ch | S]}
  end;

handle(S, {leave, Ch, Client}) ->
  % Iterates (hopefully) through all channels registered to a server and
  % stops them
  lists:foreach(fun(Ch) -> genserver:stop(list_to_atom(Ch)) end, S),
  {reply,ok,[]}.

channel(Clients, {join, Client})->
  case lists:member(Client,Clients) of 
    true -> {reply, failed, Clients};
    false -> {reply, joined, lists:append(Clients,[Client])}
end;
channel(Clients, {exit, Client})->
  case lists:member(Client,Clients) of
    true -> {reply, exited, lists:delete(Client,Clients)};
    false -> {reply, failed, Clients}
end;
channel(Clients, {message, Message, Nick, Channel, Client})-> 
  case lists:member(Client,Clients) of 
    true -> spawn(fun()->lists:foreach(
      fun(Pid) ->
        if Pid == Client -> skip;
        true -> genserver:request(Pid, {message_receive, Nick, Message, Channel, Client})
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
    genserver:request(ServerAtom, kill_channels),
    genserver:stop(ServerAtom).
