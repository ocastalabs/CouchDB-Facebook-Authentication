-module(couch_db).
-compile(export_all).

-include("couch_db.hrl").

open_int(<<"_some_db">>, []) ->
    {ok, #db{}}.

% Fixtures for various user cases
open_doc_int({db,[]},<<"org.couchdb.user:123456_non_existing_user">>,[]) ->
    something_not_ok;
open_doc_int({db,[]},<<"org.couchdb.user:888888_existing_user">>,[]) ->
    {ok, #doc{
        deleted=false,
        id=list_to_binary("org.couchdb.user:888888_existing_user"),
        body={[
            {<<"_id">>,<<"org.couchdb.user:888888_existing_user">>},
            {<<"name">>,"888888_existing_user"},
            {<<"roles">>,[]},
            {<<"type">>,<<"user">>}
        ]}
    }};
open_doc_int({db,[]},<<"org.couchdb.user:222222_prev_deleted_user">>,[]) ->
    {ok, #doc{deleted=true}}.

% The expected document update for the fixture cases
update_doc({db,[]},{doc,<<"org.couchdb.user:123456_non_existing_user">>,
     {[
        {<<"_id">>,<<"org.couchdb.user:123456_non_existing_user">>},
        {<<"fb_access_token">>,<<"MyGreatAccessToken">>},
        {<<"name">>,"123456_non_existing_user"},
        {<<"roles">>,[]},
        {<<"type">>,<<"user">>}
    ]},
    false},[]) ->
    {ok, some_new_rev};
update_doc({db,[]},{doc,<<"org.couchdb.user:222222_prev_deleted_user">>,
     {[
        {<<"_id">>,<<"org.couchdb.user:222222_prev_deleted_user">>},
        {<<"fb_access_token">>,<<"MySuperAccessToken">>},
        {<<"name">>,"222222_prev_deleted_user"},
        {<<"roles">>,[]},
        {<<"type">>,<<"user">>}
    ]},
    false},[]) ->
    {ok, some_new_rev};
update_doc({db,[]},{doc,<<"org.couchdb.user:888888_existing_user">>,
    {[
        {<<"fb_access_token">>,<<"MyOtherGreatAccessToken">>},
        {<<"type">>,<<"user">>},
        {<<"roles">>,[]},
        {<<"name">>,"888888_existing_user"},
        {<<"_id">>,<<"org.couchdb.user:888888_existing_user">>}
    ]},
    false},[]) ->
    {ok, some_new_rev}.

