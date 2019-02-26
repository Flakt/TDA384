-module(server).
-export([start/1,stop/1]).

-record(server_St, {
    nicks % list of taken nicks
}).

initial_state() ->
    #server_St{
        nicks = []
    }.

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    initial_state(),
    (catch genserver:start(ServerAtom, [], fun handle/2)).

% Handle function for the server
% This pattern match handles joining channels
handle(St, {join, Ch, Client}) ->
  % Checks if specified channel is in the server
  case lists:member(Ch, St) of
    % If true, attempts to join the channel
    true -> Result = (catch genserver:request(list_to_atom(Ch), {join, Client})),
      case Result of
        joined -> {reply, joined, St};
        failed -> {reply, failed, St}
      end;
    % if false, creates and joins the channel
    false -> (catch genserver:start(list_to_atom(Ch), [Client], fun channel/2)),
      {reply, joined, [Ch | St]}
  end;

% This pattern match handles killing all channels in the server
handle(St, kill_channels) ->
  % Iterates through all channels registered to a server and stops them
  lists:foreach(fun(Ch) -> genserver:stop(list_to_atom(Ch)) end, St),
  {reply,ok,[]};

handle(St, {change_nick, OldNick, NewNick}) ->
  io:fwrite("im in"),
  case lists:member(NewNick, St#server_St.nicks) of 
    true -> io:fwrite("true"),
      {reply, failed, St};
    false -> io:fwrite("false"),
      {reply, ok, St#server_St{nicks = [NewNick | lists:delete(OldNick, St#server_St.nicks)]}}
end.

% Handler function for channel processes
% This pattern match handles requests for joining the channel
channel(Clients, {join, Client})->
  % Checks if client is part of the channel
  case lists:member(Client,Clients) of
    true -> {reply, failed, Clients};
    false -> {reply, joined, [Client | Clients]}
  end;

% This pattern match handles requests for leaving the channel
channel(Clients, {leave, Client})->
    % Checks if client is part of the channel
  case lists:member(Client,Clients) of
    true -> {reply, ok, lists:delete(Client,Clients)};
    false -> {reply, failed, [Clients]}
  end;

% This pattern match handles requests for messaging all clients in the channel
channel(Clients, {message, Channel, Nick, Msg, Client})->
    % Checks if client is part of the channel
  case lists:member(Client,Clients) of
    % Spawns a process for each client in the channel that sends a
    % message_receive to genserver for each client.
    true -> spawn(fun()->lists:foreach(
      fun(Pid) ->
        % No need to message yourself
        if Pid == Client -> skip;
        true -> genserver:request(Pid, {message_receive, Channel, Nick, Msg})
        end
      end,
    Clients) end),
    {reply, ok, Clients};
    false -> {reply, failed, Clients}
  end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, kill_channels),
    genserver:stop(ServerAtom).
