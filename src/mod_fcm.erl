-module(mod_fcm).

-behaviour(gen_mod).

%% gen_mod API callbacks
-export([start/2, stop/1, reload/3, depends/2]).
-export([offline_message/1, adhoc_commands/4]).

%% Required by ?INFO_MSG macros
-include("logger.hrl").
-include("xmpp.hrl").
-include("adhoc.hrl").

adhoc_commands(Acc, From, To, #adhoc_command{node = Command, action = execute, xdata = XData} = Req) ->
  Host = To#jid.lserver,
  ?INFO_MSG("mod_fcm adhoc_commands, ~s", [Command]),
  Result = adhoc_perform_action(Host, Command, From, XData),
  ?INFO_MSG("mod_fcm adhoc_commands result, ~s : ~p", [Command, Result]),

  case Result of
    unknown -> Acc;
    {error, Error} -> {error, Error};

    ok ->
      xmpp_util:make_adhoc_response(Req, #adhoc_command{status = completed})

  end;
adhoc_commands(Acc, _From, _To, Request) ->
  ?INFO_MSG("mod_fcm adhoc_commands 2,  : ~p", [Request]),
  Acc.

adhoc_perform_action(Host, <<"register-push-user">>, #jid{luser = User}, XData) ->
  FNick = xmpp_util:get_xdata_values(<<"nick">>, XData),
  FToken = xmpp_util:get_xdata_values(<<"token">>, XData),
  case FNick ++ FToken of
    [Nick, Token] -> mod_fcm_sql:register_user(Host, User, Nick, Token);
    _ -> {error, xmpp:err_bad_request()}
  end;

adhoc_perform_action(Host, <<"register-push-token">>, #jid{luser = User}, XData) ->
  case xmpp_util:get_xdata_values(<<"token">>, XData) of
    [Token] -> mod_fcm_sql:register_token(Host, User, Token);
    _ -> {error, xmpp:err_bad_request()}
  end;

adhoc_perform_action(Host, <<"update-push-nick">>, #jid{luser = User}, XData) ->
  case xmpp_util:get_xdata_values(<<"nick">>, XData) of
    [Nick] -> mod_fcm_sql:update_nick(Host, User, Nick);
    _ -> {error, xmpp:err_bad_request()}
  end;

adhoc_perform_action(Host, <<"unregister-push-token">>, #jid{luser = User}, XData) ->
  case xmpp_util:get_xdata_values(<<"token">>, XData) of
    [Token] -> mod_fcm_sql:unregister_token(Host, User, Token);
    _ -> {error, xmpp:err_bad_request()}
  end.

offline_message({_, #message{to = To, from = From, body = Body, thread = Thread}} = Acc) ->
  FromUser = From#jid.luser,
  ToUser = To#jid.luser,
  Host = To#jid.lserver,
  Message = xmpp:get_text(Body),
  Info = mod_fcm_sql:get_push_data(Host, FromUser, ToUser),

  ?INFO_MSG("mod_fcm push info: '~p'", [Info]),

  case Info of
    {selected, [SenderNick, ReceiverTokens]} ->
      send_notification(string:split(ReceiverTokens, <<",">>, all), Message, SenderNick, Thread, FromUser);
    _ -> ok
  end,

  Acc.

send_notification([], Message, Title, Thread, Tag) ->
  ?INFO_MSG("mod_fcm no tokens found", []);

send_notification(Tokens, Message, Title, Thread, Tag) ->
  fcm:push(
    swapp_fcm,
    Tokens,
    [
      {
        <<"data">>,
        [
          {<<"thread">>, Thread}
        ]
      },
      {
        <<"notification">>,
        [
          {<<"title">>, Title},
          {<<"body">>, Message},
          {<<"tag">>, Tag}
        ]
      },
      {<<"priority">>, <<"high">>}
    ]
  ),
  ?INFO_MSG("mod_fcm sent from ~s (~s) to ~p message '~s' (~s)", [Title, Tag, Tokens, Message, Thread]).

start(Host, _Opts) ->
  ?INFO_MSG("mod_fcm staring...", []),
  FcmKey = os:getenv("FCM_KEY"),
  fcm_app:start(normal, []),
  ?INFO_MSG("mod_fcm key = ~s", [FcmKey]),
  %% mod_push has priority of 50
  ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_message, 45),
  ejabberd_hooks:add(adhoc_local_commands, Host, ?MODULE, adhoc_commands, 45),
  ?INFO_MSG("mod_fcm added offline hook", []),
  fcm:start(swapp_fcm, FcmKey),
  ?INFO_MSG("mod_fcm started", []),
  ok.

stop(Host) ->
  ?INFO_MSG("mod_fcm stoping...", []),
  ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, offline_message, 45),
  ejabberd_hooks:delete(adhoc_local_commands, Host, ?MODULE, adhoc_commands, 45),
  ?INFO_MSG("mod_fcm stopping fcm service...", []),
  fcm:stop(swapp_fcm),
  ?INFO_MSG("mod_fcm stopped", []),
  ok.

reload(_Host, _NewOpts, _OldOpts) ->
  ok.

depends(_Host, _Opts) ->
  [].
