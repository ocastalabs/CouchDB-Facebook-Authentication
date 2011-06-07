-module(couch_config).
-compile(export_all).

get("couch_httpd_auth", "secret", _Default) ->
    "something";

get("fb", "redirect_uri", _Default) ->
    "some redirect_uri";
get("fb", "client_id", _Default) ->
    "some client_id";
get("fb", "client_secret", _Default) ->
    "some client_secret";
get("fb","destination_db",null) ->
    "_the_users_db";
get("fb","client_app_uri",nil) ->
    "http://example.com/superapp?";

get(_,_,_) ->
    undefined.
