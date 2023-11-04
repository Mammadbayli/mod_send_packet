-module(mod_send_packet).

-behaviour(gen_mod).

-export([start/2,
        stop/1, 
        mod_options/1,
        mod_opt_type/1, 
        depends/2, 
        mod_doc/0]).

-export([process_packet/1]).

-include("scram.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
%% Required by ?T macro
-include("translate.hrl"). 

start(Host, _Opts) ->
    ?INFO_MSG("starting mod_send_packet", []),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
                       process_packet, 50),
    ok.

stop(Host) ->
    ?INFO_MSG("stopping mod_send_packet", []),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
                          process_packet, 50),
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

process_packet({#message{type = T} = Packet, _C2SState} = Acc)
  when T == chat; T == groupchat ->
    ?INFO_MSG("Send packet", []),
    ok = filter_message(Packet),
    Acc;

process_packet(Acc) ->
    Acc.

filter_message(#message{from = _From, to = _To, body = Body} = Msg) ->
    case xmpp:get_text(Body) of
	<<"">> ->
	    ok;
	_ ->
           forward_message(Msg, 1)
    end.

forward_message(Packet, BadgeCount) ->
    Host = (Packet#message.to)#jid.lserver,
    
    RemoteUrl = gen_mod:get_module_opt(Host, ?MODULE, remote_url),
    Token = gen_mod:get_module_opt(Host, ?MODULE, auth_token),
  
    %%Stanza = fxml:element_to_binary(xmpp:encode(Packet)),
     Stanza = binary_to_list(fxml:element_to_binary(xmpp:encode(Packet))),
     
     Data = string:join(["{",
        "\"stanza\": \"", Stanza, "\", ",
        "\"badge\": \"", integer_to_list(BadgeCount), "\"",
    "}"], ""),

    Request = {binary_to_list(RemoteUrl), [{"Authorization", binary_to_list(Token)}], "application/json", Data},
    httpc:request(post, Request,[],[]).
