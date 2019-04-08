-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels % list of the client channels
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = []
    }.

% Join Channel
handle(St, {join, Channel}) ->
  % Checks if server is active
  case lists:member(St#client_st.server, registered()) of
    % If server is active, tries to join the specified channel
    true -> Result = (catch genserver:request(St#client_st.server, {join, Channel, self()})),
      case Result of
        {'EXIT',_} -> {reply, {error, server_not_reached, "Server does not respond"}, St};
        joined -> {reply, ok, St#client_st{channels = lists:append(St#client_st.channels, [Channel])}};
        failed -> {reply, {error, user_already_joined, "Already in channel"}, St}
      end;
    % Server is not active, return error
    false -> {reply, {error, server_not_reached, "Server unreachable"}, St}
  end;

% Leave channel
handle(St, {leave, Channel}) ->
    % Checks if client is in channel
    case lists:member(Channel, St#client_st.channels) of
      % If in channel, tries to leave
      true -> (catch genserver:request(list_to_atom(Channel), {leave, self()})),
        {reply, ok, St#client_st{channels =
         lists:delete(Channel, St#client_st.channels) }};
      % If not in channel, returns error
      false -> {reply, {error, user_not_joined, "User not in channel"}, St}
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    % Sends request to genserver for messaging all clients in Channel
    Result = (catch genserver:request(list_to_atom(Channel), {message, Channel,
    St#client_st.nick, Msg, self()})),
    case Result of
      {'EXIT',_} -> {reply, {error, server_not_reached, "Server does not respond"}, St};
      ok -> {reply, ok, St};
      failed -> {reply, {error, user_not_joined, "Not in the channel"} , St}
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
