-module(server).
-export([start/1,stop/1]).

-record(server, {server}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
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
        failed -> {reply, failed, S}
      end;
    false -> genserver:start(list_to_atom(Ch), S, channel),
      {reply, join
  end;
  not_implemented.

channel() ->

handle(S, kill_channels) ->
  % Iterates (hopefully) through all channels registered to a server and
  % stops them
  lists:foreach(fun(Ch) -> genserver:stop(list_to_atom(Ch)) end, S),
  {reply,ok,[]}.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:request(ServerAtom, kill_channels),
    genserver:stop(ServerAtom).
