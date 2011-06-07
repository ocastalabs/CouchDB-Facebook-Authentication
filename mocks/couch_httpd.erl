-module(couch_httpd).
-compile(export_all).

-include("couch_db.hrl").

send_json(Req, Code, Headers, Body) ->
    { send_json_called, 
        { req,      Req },
        { code,     Code },
        { headers,  Headers },
        { body,     Body }
    }.

qs_value(Req, "clientapptoken") ->
    Req#httpd.clientapptoken.

