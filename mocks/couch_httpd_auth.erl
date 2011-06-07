-module(couch_httpd_auth).
-compile(export_all).

cookie_auth_header(_Req, []) ->
    <<"AuthSession=SomeTokenHere">>.
