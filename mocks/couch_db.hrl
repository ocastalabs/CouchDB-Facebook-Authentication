% Based on couch.hrl
% Only stubs what's used by the module.

-define(b2l(V), binary_to_list(V)).
-define(l2b(V), list_to_binary(V)).

-define(LOG_DEBUG(Format, Args), io:format(Format, Args)).
-define(LOG_INFO(Format, Args) , io:format(Format, Args)).
-define(LOG_ERROR(Format, Args), io:format(Format, Args)).

-record(httpd,
    {
    method,
    user_ctx,
    auth,
    clientapptoken
    }).

-record(doc,
    {
    id = <<"">>,
    body = {[]},
    deleted = false
    }).

-record(user_ctx,
    {
    name=null,
    roles=[],
    handler
    }).

-record(db,
    {
    validate_doc_funs = []
    }).

