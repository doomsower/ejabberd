-module(mod_fcm_sql).

-compile([{parse_transform, ejabberd_sql_pt}]).

%% gen_mod API callbacks
-export([register_user/4, register_token/3, unregister_token/3, update_nick/3, get_push_data/3]).

%% Required by ?INFO_MSG macros
-include_lib("stdlib/include/ms_transform.hrl").
-include("logger.hrl").
-include("ejabberd_sql_pt.hrl").

register_user(LServer, User, Nick, Token) ->
  case ejabberd_sql:sql_query(LServer,
    ?SQL("INSERT INTO push_info(username, nick, tokens) "
         "VALUES (%(User)s, %(Nick)s, { %(Token)s }) "
         "ON CONFLICT DO UPDATE SET "
         "nick = EXCLUDED.nick, "
         "tokens = insert_unique(tokens, %(Token)s)")) of
    {error, Reason} -> {error, Reason};
    _ -> ok
  end.

update_nick(LServer, User, Nick) ->
  case ejabberd_sql:sql_query(LServer,
    ?SQL("UPDATE push_info SET nick = %(Nick)s WHERE username = %(User)s")) of
    {error, Reason} -> {error, Reason};
    _ -> ok
  end.

register_token(LServer, User, Token) ->
  case ejabberd_sql:sql_query(LServer,
    ?SQL("UPDATE push_info "
         "SET tokens = insert_unique(tokens, %(Token)s) "
         "WHERE username = %(User)s"
    )) of
    {error, Reason} -> {error, Reason};
    _ -> ok
  end.

unregister_token(LServer, User, Token) ->
  case ejabberd_sql:sql_query(LServer,
    ?SQL("UPDATE push_info "
         "SET tokens = array_remove(tokens, %(Token)s) "
         "WHERE username = %(User)s"
    )) of
    {error, Reason} -> {error, Reason};
    _ -> ok
  end.

get_push_data(LServer, From, To) ->
  ejabberd_sql:sql_query(LServer,
    ?SQL("SELECT "
         "(SELECT nick FROM push_info WHERE username = %(From)s) as nick "
         "(SELECT tokens FROM push_info WHERE username = %(To)s) as tokens")).
