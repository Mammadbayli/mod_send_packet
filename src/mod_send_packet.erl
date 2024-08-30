-module(mod_send_packet).

-behaviour(gen_mod).

-export([start/2,
         stop/1, 
         mod_options/1,
         mod_opt_type/1, 
         depends/2, 
         mod_doc/0]).

-export([process_packet/1,
	 process_packet_offline/1]).

-include("scram.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
-include("translate.hrl"). 
-include("mod_muc_room.hrl").

start(Host, _Opts) ->
    ?INFO_MSG("starting mod_send_packet", []),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE, process_packet, 50),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, process_packet_offline, 50),
    ok.

stop(Host) ->
    ?INFO_MSG("stopping mod_send_packet", []),
     ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, process_packet, 50),
     ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, process_packet_offline, 50),
     ok.

depends(_Host, _Opts) ->
    [].

mod_doc() ->
    #{desc =>
            ?T("Forward messages to HTTP endpoint.")}.

mod_options(_Host) ->
  [{auth_token, <<"secret">>},
  {remote_url, <<"http://example.com/test">>}].

mod_opt_type(auth_token) ->
  fun iolist_to_binary/1;

mod_opt_type(remote_url) ->
  fun iolist_to_binary/1.

process_packet({#message{type = T} = Packet, _C2SState} = Acc) when T == chat ->
    ?INFO_MSG("Send packet hook", []),
    Vhost = (Packet#message.to)#jid.lserver,
    BadgeCount = mod_offline:get_queue_length((Packet#message.to)#jid.luser, Vhost),
    forward_message(Packet, BadgeCount),
    Acc;

process_packet(Acc) ->
    Acc.

process_packet_offline({Action, Packet} = Acc) when (Packet#message.type == chat) ->
    ?INFO_MSG("Send packet offline chat", []),
    Acc;

process_packet_offline({Action, Pkt} = Acc) ->
    ?INFO_MSG("Send packet offline groupchat", []),
    Packet = unwrap_message(Pkt),
    Vhost = (Packet#message.to)#jid.lserver,
    BadgeCount = mod_offline:get_queue_length((Packet#message.to)#jid.luser, Vhost),
    forward_message(Packet, BadgeCount),
    Acc;

process_packet_offline(Acc) ->
    Acc.

unwrap_message(#message{type = normal} = Msg) ->
    case misc:unwrap_mucsub_message(Msg) of
	#message{} = InnerMsg ->
	    InnerMsg;
	false ->
	    Msg
    end;

unwrap_message(Stanza) ->
    Stanza.

forward_message(Packet, BadgeCount) ->
    To = Packet#message.to,

    RemoteUrl = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, remote_url),
    Token = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, auth_token),
  
    Stanza = binary_to_list(fxml:element_to_binary(xmpp:encode(Packet))),
     
     Data = string:join(["{",
        "\"stanza\": \"", Stanza, "\", ",
        "\"badge\": \"", integer_to_list(BadgeCount), "\"",
    "}"], ""),

    Request = {binary_to_list(RemoteUrl), [{"Authorization", binary_to_list(Token)}], "application/json", Data},
    httpc:request(post, Request,[],[{sync, false}]).

