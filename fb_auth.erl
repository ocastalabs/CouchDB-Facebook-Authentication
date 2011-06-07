-module(fb_auth).

-export([handle_fb_req/1]).
-export([handle_fb_resp_req/1]).

-include("couch_db.hrl").




%% Exported functions

handle_fb_req(#httpd{method='GET'}=Req) ->
    % Get values from the config file
    [Scope, ClientID, RedirectURI] = get_config(["scope", "client_id", "redirect_uri"]),

    % extract the client apps token so that we can pass it thru the facebook api
    % (this allows the client app to pass some state)
    FullRedirectUrl = case get_encoded_client_app_redirect_from_qs(Req) of
        "" ->
            couch_util:url_encode(RedirectURI);
        CAT ->
            couch_util:url_encode(RedirectURI++"?clientapptoken="++CAT)
    end,
    % Construct the FB oauth URL
    Url = "https://www.facebook.com/dialog/oauth?client_id="++ClientID++"&scope="++Scope++"&redirect_uri="++FullRedirectUrl,
    ?LOG_DEBUG("handle_fb_req - redirecting to ~p", [Url]),

    % Redirect the client to the FB Oauth page
    couch_httpd:send_json(Req, 302, [{"Location", Url}], {[]});

handle_fb_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET").


handle_fb_resp_req(#httpd{method='GET'}=Req) ->
    % Did we get a 'code' or 'error' back from facebook?
    case couch_httpd:qs_value(Req, "code") of
        undefined ->
            ?LOG_DEBUG("Facebook responded with something other than a code: ~p", [Req]),
            couch_httpd:send_json(Req, 403, [], {[{error, <<"No code supplied">>}]});
        Code ->
            handle_fb_code(Req, Code)
    end;

handle_fb_resp_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET").


%% Private and Utility functions

get_unmodified_client_app_redirect_from_qs(Req) ->
    case couch_httpd:qs_value(Req, "clientapptoken") of
        undefined -> "";
        Cat -> Cat
    end.

get_encoded_client_app_redirect_from_qs(Req) ->
    couch_util:url_encode( get_unmodified_client_app_redirect_from_qs(Req) ).

handle_fb_code(Req, FBCode) ->
    % Extract required values from config ini
    [RedirectURI, ClientID, ClientSecret, DestDBName] = get_config(["redirect_uri", "client_id", "client_secret", "destination_db"]),

    % if the client passed in a client app token then facebook should have passed it back to us,
    % so extract it.
    ClientAppToken = get_encoded_client_app_redirect_from_qs(Req),

    % Get an access token from Facebook
    case request_facebook_access_token(ClientAppToken, RedirectURI, ClientID, ClientSecret, FBCode) of
        {ok, AccessToken} ->
            % Retrieve info from the graph/me API call
            case request_facebook_graphme_info(AccessToken) of
                {ok, ID} ->
                    % Create or update user auth doc with access token
                    case update_or_create_user_doc(DestDBName, ID, AccessToken) of
                        {ok, Rev} ->
                            ?LOG_DEBUG("Updated user doc for facebook id ~p is rev ~p", [ID, Rev]),
                            % Finally send a response that includes the AuthSession cookie
                            generate_cookied_response_json(ID, Req, AccessToken);
                        Error ->
                            ?LOG_DEBUG("Non-success from update_or_create_user_doc call: ~p", [Error]),
                            couch_httpd:send_json(Req, 403, [], {[{error, <<"Unable to update doc">>}]})
                    end;
                Error ->
                    ?LOG_DEBUG("Non-success from request_facebook_graphme_info call: ~p", [Error]),
                    couch_httpd:send_json(Req, 403, [], {[{error, <<"Failed graphme request">>}]})
            end;
        Error ->
            ?LOG_DEBUG("Non-success from request_facebook_access_token call: ~p", [Error]),
            couch_httpd:send_json(Req, 403, [], {[{error, <<"Could not get access token">>}]})
    end.


get_config(Keys) ->
    lists:map(
        fun(K) ->
            case couch_config:get("fb", K, undefined) of
                undefined ->
                    throw({missing_config_value, "Cannot find key '"++K++"' in [fb] section of config"});
                Any ->
                    Any
            end
        end, Keys).


generate_cookied_response_json(ID, Req, AccessToken) ->
    % Create an auth cookie in the same way that couch_httpd_auth.erl does.
    % NOTE: This could be fragile! If couch_httpd_auth.erl changes the way it handles
    %       auth cookie then this code will break. However, couch_httpd_auth.erl doesn't
    %       seem to expose enough method for us to make it do all the work.
    User = case couch_auth_cache:get_user_creds(ID) of
        nil -> [];
        Result -> Result
    end,
    UserSalt = couch_util:get_value(<<"salt">>, User, <<>>),
    Secret=?l2b( case couch_config:get("couch_httpd_auth", "secret", nil) of
        nil ->
            NewSecret = ?b2l(couch_uuids:random()),
            couch_config:set("couch_httpd_auth", "secret", NewSecret),
            NewSecret;
        Sec -> Sec
    end ),

    % Create a json response containing some useful info and the AuthSession
    % cookie.
    ClientAppUri = couch_config:get("fb", "client_app_uri", nil),
    couch_httpd:send_json(Req, 302,
        [{"Location", ClientAppUri++get_unmodified_client_app_redirect_from_qs(Req) }] ++
        couch_httpd_auth:cookie_auth_header(Req#httpd{user_ctx=#user_ctx{name=ID}, auth={<<Secret/binary, UserSalt/binary>>, true}}, []),
        {[
            {fbid, ID},
            {access_token, ?l2b(AccessToken)}
        ]}
    ).

update_or_create_user_doc(DestDBName, ID, AccessToken) ->
    % Generate a _users compatible ID
    FullID=?l2b("org.couchdb.user:"++ID),

    % Open the database
    {ok, Db} = couch_db:open_int(?l2b(DestDBName), []),

    % Read and ammend existing doc, or create a new one
    NewDoc = case couch_db:open_doc_int(Db, FullID, []) of
        {ok, #doc{deleted=false}=OrigDoc} ->
            ?LOG_INFO("Updating user doc in ~p for facebook id ~p (couch id ~p). New access token is ~p", [DestDBName, ID, FullID, AccessToken]),
            OrigDoc#doc{
                body=couch_util:json_apply_field({?l2b("fb_access_token"), ?l2b(AccessToken)}, OrigDoc#doc.body)
            };
        _ ->
            ?LOG_INFO("No user doc found in ~p for facebook id ~p (couch id ~p), creating a new one.", [DestDBName, ID, FullID]),
            #doc{
                id=FullID,
                body={[
                    {?l2b("_id"), FullID},
                    {?l2b("fb_access_token"), ?l2b(AccessToken)},
                    {?l2b("name"), ID},
                    {?l2b("roles"), []},
                    {?l2b("type"), ?l2b("user")}
                ]}
            }
    end,

    % To prevent the validation functions for the db taking umbridge at our
    % behind the scenes twiddling, we blank them out.
    % NOTE: Potentially fragile. Possibly dangerous?
    % TODO: Make this configurable?
    % TODO: If nothing has changed, there's no need for a write. For now, we write anyway.
    DbWithoutValidationFunc = Db#db{ validate_doc_funs=[] },
    couch_db:update_doc(DbWithoutValidationFunc, NewDoc, []).


request_facebook_graphme_info(AccessToken) ->
    % Construct the URL to access the graph API's /me page
    Url="https://graph.facebook.com/me?access_token="++AccessToken,
    ?LOG_DEBUG("Url=~p",[Url]),

    % Request the page
    Resp=http:request(Url),
    ?LOG_DEBUG("request_facebook_graphme_info response=~p",[Resp]),

    process_facebook_graphme_response(Resp).

process_facebook_graphme_response(Resp) ->
    % Extract user facebook id from the body
    case Resp of 
        {ok, {{_,200,_}, _, Body}} ->
            % Decode the facebook response body, extracting the
            % ID and the complete response.
            {FBInfo}=couch_util:json_decode(Body),
            ID=couch_util:get_value(<<"id">>, FBInfo),
            {ok, ID};
        _ ->
            {error, "Non 200 response from facebook"}
    end.


request_facebook_access_token(ClientAppToken, RedirectURI, ClientID, ClientSecret, FBCode) ->
    % Construct the access token request URL.
    % NOTE: We do not use type=client_type because if we do then we don't get a
    % session access code back, and without that we are unable to use the /me
    % alias of the graph API. The redirect_uri is ignored by us, but mandated
    % by the API.

    FullRedirectUrl = case ClientAppToken of
        "" ->
            couch_util:url_encode(RedirectURI);
        CAT ->
            couch_util:url_encode(RedirectURI++"?clientapptoken="++CAT)
    end,
    Url="https://graph.facebook.com/oauth/access_token?&client_id="++ClientID++"&client_secret="++ClientSecret++"&code="++FBCode++"&redirect_uri="++FullRedirectUrl,
    ?LOG_DEBUG("request_facebook_access_token: requesting using URL - ~p", [Url]),

    % Request the page
    Resp=http:request(Url),
    ?LOG_DEBUG("Full response from Facebook: ~p", [Resp]),

    process_facebook_access_token(Resp).


process_facebook_access_token(Resp) ->
    % Extract the info we need
    case Resp of 
        {ok, {{_,200,_}, _, Body}} ->
            case string:tokens(Body, "=") of
                ["access_token", AccessToken] ->
                    ?LOG_DEBUG("process_facebook_access_token: access_token=~p",[AccessToken]),
                    {ok, AccessToken};
                _ ->
                    ?LOG_DEBUG("process_facebook_access_token: unexpected response: ~p", [Body]),
                    {error, "Unexpected body response from facebook"}
            end;
        _ ->
            ?LOG_DEBUG("process_facebook_access_token: non 200 response of: ~p", [Resp]),
            {error, "Non 200 response from facebook"}
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Unit Tests - compilation conditional, see Makefile %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

generate_cookied_response_json_test() ->
    {
        send_json_called, {req, _Req}, {code, Code}, {headers, Headers}, {body, Body}
    } = generate_cookied_response_json("12345", #httpd{method='GET', user_ctx=[], auth=[], clientapptoken="SomePlace"}, "some_access_token"),
    ?assertEqual(302, Code),
    ?assertEqual([{"Location","http://example.com/superapp?SomePlace"}| <<"AuthSession=SomeTokenHere">>], Headers),
    ?assertEqual({[{fbid,"12345"},{access_token,<<"some_access_token">>}]}, Body).

process_facebook_graphme_response__success_test() ->
    % success path
    ?assertEqual(
        {ok, "AnIdFromFacebook"},
        process_facebook_graphme_response({ok, {{blah,200,blah}, blah, "JsonStringFbGraphMeResponse"}})
    ).
process_facebook_graphme_response__non_200_response_code_test() ->
    % faulure path - unexpected response code
    ?assertEqual(
        {error, "Non 200 response from facebook"},
        process_facebook_graphme_response({ok, {{blah,404,blah}, blah, "JsonStringFbGraphMeResponse"}})
    ).
process_facebook_graphme_response__non_ok_test() ->
    % faulure path - non-ok from httpc
    ?assertEqual(
        {error, "Non 200 response from facebook"},
        process_facebook_graphme_response({error, some_kind_of_error})
    ).

request_facebook_access_token__success__test() ->
    % success path
    ?assertEqual(
        {ok, "GroovyAccessToken"},
        process_facebook_access_token({ok, {{blah,200,blah}, blah, "access_token=GroovyAccessToken"}})
    ).
request_facebook_access_token__bad_body__test() ->
    % failure path - bad body content
    ?assertEqual(
        {error, "Unexpected body response from facebook"},
        process_facebook_access_token({ok, {{blah,200,blah}, blah, "something_unexpected"}})
    ).
request_facebook_access_token__non_200_response_code__test() ->
    % failure path - unexpected response code
    ?assertEqual(
        {error, "Non 200 response from facebook"},
        process_facebook_access_token({ok, {{blah,876,blah}, blah, "access_token=GroovyAccessToken"}})
    ).
request_facebook_access_token__non_ok__test() ->
    % failure path - non-ok from httpc
    ?assertEqual(
        {error, "Non 200 response from facebook"},
        process_facebook_access_token({error, some_kind_of_error})
    ).

% NOTE: See mocks/couch_db.erl to see what makes these tests tick. It makes heavy use of
%       fixures and pattern matching. Thus *** it's likely to fail with badmatch rather
%       than an assert failure if the implementation changes ***.
update_or_create_user_doc__non_existing_user_test() ->
    % Non existing user
    ?assertEqual(
        {ok, some_new_rev},
        update_or_create_user_doc("_some_db", "123456_non_existing_user", "MyGreatAccessToken")
    ).
update_or_create_user_doc__prev_del_user_test() ->
    % A previusly deleted users record will be found by open_doc_int UNLESS
    % you specifically make sure the deleted flag is set to false.
    ?assertEqual(
        {ok, some_new_rev},
        update_or_create_user_doc("_some_db", "222222_prev_deleted_user", "MySuperAccessToken")
    ).
update_or_create_user_doc__existing_user_test() ->
    % Existing user
    ?assertEqual(
        {ok, some_new_rev},
        update_or_create_user_doc("_some_db", "888888_existing_user", "MyOtherGreatAccessToken")
    ).

get_config__success_test() ->
    ?assertEqual(
        ["some redirect_uri","some client_id","some client_secret"],
        get_config(["redirect_uri", "client_id", "client_secret"])
    ).
get_config__missing_test() ->
    ?assertThrow(
        {missing_config_value, "Cannot find key 'DOES NOT EXIST' in [fb] section of config"},
        get_config(["redirect_uri", "DOES NOT EXIST", "client_secret"])
    ).

get_encoded_client_app_redirect_from_qs_test() ->
    % make sure couch_util:url_encode is called on the string (see mock)
    Req=#httpd{clientapptoken="FooBar"},
    ?assertEqual(
        "url_encode(FooBar)",
        get_encoded_client_app_redirect_from_qs(Req)
    ).

get_unmodified_client_app_redirect_from_qs_undef_test() ->
    ?assertEqual(
        "",
        get_unmodified_client_app_redirect_from_qs(#httpd{})
    ).

get_unmodified_client_app_redirect_from_qs_test() ->
    ?assertEqual(
        "Meow!",
        get_unmodified_client_app_redirect_from_qs(#httpd{ clientapptoken="Meow!" })
    ).

-endif.
