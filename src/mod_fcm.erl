-module(mod_fcm).

-behaviour(gen_mod).

%% gen_mod API callbacks
-export([start/2, stop/1, reload/3, depends/2]).
-export([offline_message_hook/1]).

%% Required by ?INFO_MSG macros
-include("logger.hrl").
-include("xmpp.hrl").

start(Host, _Opts) ->
  ?INFO_MSG("mod_fcm staring...", []),
  FcmKey = os:getenv("FCM_KEY"),
  fcm_app:start(normal, []),
  ?INFO_MSG("mod_fcm key = ~s", [FcmKey]),
  %% mod_mam has priority of 50
  ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_message_hook, 60),
  ?INFO_MSG("mod_fcm added offline hook", []),
  fcm:start(swapp_fcm, FcmKey),
  ?INFO_MSG("mod_fcm started", []),
  ok.

offline_message_hook({_Action, #message{to = To, from = From, body = Body}} = Acc) ->
  ToResource = To#jid.lresource,
  FromUser = From#jid.luser,
  Message = xmpp:get_text(Body),
  ?INFO_MSG("mod_fcm pushing, '~s' from '~s' to '~s'", [Message, FromUser, ToResource]),
  
  if
    ToResource /= <<>> ->
      fcm:push(swapp_fcm, [ToResource], [{<<"data">>, [{<<"message">>, Message}]}]),
      ok;
    true -> ok
  end,
  
  ?INFO_MSG("mod_fcm pushed, '~s' from '~s' to '~s'", [Message, FromUser, ToResource]),
  Acc.

stop(Host) ->
  ?INFO_MSG("mod_fcm stoping...", []),
  ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, offline_message_hook, 60),
  ?INFO_MSG("mod_fcm stopping fcm service...", []),
  fcm:stop(swapp_fcm),
  ?INFO_MSG("mod_fcm stopped", []),
  ok.

reload(_Host, _NewOpts, _OldOpts) ->
  ok.

depends(_Host, _Opts) ->
  [].
