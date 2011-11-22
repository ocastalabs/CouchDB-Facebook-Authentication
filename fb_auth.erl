-module(fb_auth).
-export([handle_fb_req/1]).
-include("couch_db.hrl").

handle_fb_req(#httpd{method='GET'}=Req) ->
    % Did we get a 'code' or 'error' back from facebook?
    case couch_httpd:qs_value(Req, "code") of
        undefined ->
	    ?LOG_DEBUG("Facebook responded with something other than a code: ~p", [Req]),
	    couch_httpd:send_json(Req, 403, [], {[{error, <<"No code supplied">>}]});
        Code -> handle_fb_code(Req, Code)
    end;

handle_fb_req(Req) ->
    couch_httpd:send_method_not_allowed(Req, "GET").

handle_fb_code(Req, FBCode) ->
    % Extract required values from config ini
    [RedirectURI, ClientID, ClientSecret, DestDBName] = lists:map(fun(K) ->
									  case couch_config:get("fb", K, undefined) of
									      undefined -> throw({missing_config_value, "Cannot find key '"++K++"' in [fb] section of config"});
									      V -> V
									  end
								  end, ["redirect_uri", "client_id", "client_secret", "destination_db"]),

    % if the client passed in a client app token then facebook should have passed it back to us,
    % so extract it.
    ClientAppToken = case couch_httpd:qs_value(Req, "clientapptoken") of
        undefined -> "";
        Cat -> couch_util:url_encode(Cat)
    end,

    % Get an access token from Facebook
    case request_facebook_access_token(ClientAppToken, RedirectURI, ClientID, ClientSecret, FBCode) of
        {ok, AccessToken} ->
            % Retrieve info from the graph/me API call
            case request_facebook_graphme_info(AccessToken) of
                {ok, ID} ->
		    create_user_doc(DestDBName, ID),
                    generate_cookie(ID, Req);
                Error ->
                    ?LOG_DEBUG("Non-success from request_facebook_graphme_info call: ~p", [Error]),
                    couch_httpd:send_json(Req, 403, [], {[{error, <<"Failed graphme request">>}]})
            end;
        Error ->
            ?LOG_DEBUG("Non-success from request_facebook_access_token call: ~p", [Error]),
            couch_httpd:send_json(Req, 403, [], {[{error, <<"Could not get access token">>}]})
    end.

generate_cookie(ID, Req) ->
    % Create an auth cookie in the same way that couch_httpd_auth.erl does.
    % NOTE: This could be fragile! If couch_httpd_auth.erl changes the way it handles
    %       auth cookie then this code will break. However, couch_httpd_auth.erl doesn't
    %       seem to expose enough method for us to make it do all the work.
    User = case couch_auth_cache:get_user_creds(ID) of
	       nil -> [];
	       Result -> Result
	   end,
    UserSalt = couch_util:get_value(<<"salt">>, User, <<>>),
    Secret=?l2b(case couch_config:get("couch_httpd_auth", "secret", nil) of
		    nil ->
			NewSecret = ?b2l(couch_uuids:random()),
			couch_config:set("couch_httpd_auth", "secret", NewSecret),
			NewSecret;
		    Sec -> Sec
		end),

    % Create a json response containing some useful info and the AuthSession
    % cookie.
    ClientAppUri = couch_config:get("fb", "client_app_uri", nil),
    Cookie = couch_httpd_auth:cookie_auth_header(Req#httpd{user_ctx=#user_ctx{name=ID},auth={<<Secret/binary,UserSalt/binary>>,true}},[]),
    couch_httpd:send_json(Req, 302, [{"Location", ClientAppUri}] ++ Cookie, nil).

create_user_doc(DestDBName, ID) ->
    % Generate a _users compatible ID
    FullID=?l2b("org.couchdb.user:"++ID),

    % Open the database
    {ok, Db} = couch_db:open_int(?l2b(DestDBName), []),

    % Read and ammend existing doc, or create a new one
    case couch_db:open_doc_int(Db, FullID, []) of
        {ok, #doc{deleted=false}=OrigDoc} -> 
	    ?LOG_INFO("User doc found in ~p for facebook id ~p (couch id ~p).", [DestDBName, ID, FullID]),
	    OrigDoc;
        _ ->
            ?LOG_INFO("No user doc found in ~p for facebook id ~p (couch id ~p), creating a new one.", [DestDBName, ID, FullID]),
            Salt=couch_uuids:random(),
            NewDoc=#doc{
	      id=FullID,
	      body={[
		     {?l2b("_id"), FullID},
		     {?l2b("name"), ID},
		     {?l2b("roles"), []},
        	     {?l2b("salt"), Salt},             
		     {?l2b("type"), ?l2b("user")}
		    ]}
	    },
	    DbWithoutValidationFunc = Db#db{ validate_doc_funs=[] },
	    couch_db:update_doc(DbWithoutValidationFunc, NewDoc, [])	    
    end.

request_facebook_graphme_info(AccessToken) ->
    % Construct the URL to access the graph API's /me page
    Url="https://graph.facebook.com/me?access_token="++AccessToken,
    ?LOG_DEBUG("Url=~p",[Url]),

    % Request the page
    Resp=httpc:request(Url),
    ?LOG_DEBUG("request_facebook_graphme_info response=~p",[Resp]),

    process_facebook_graphme_response(Resp).

process_facebook_graphme_response(Resp) ->
    % Extract user facebook id from the body
    case Resp of 
        {ok, {{_,200,_}, _, Body}} ->
            % Decode the facebook response body, extracting the
            % ID and the complete response.
            {FBInfo}=?JSON_DECODE(Body),
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
        "" -> couch_util:url_encode(RedirectURI);
        Cat -> couch_util:url_encode(RedirectURI++"?clientapptoken="++Cat)
    end,
    Url="https://graph.facebook.com/oauth/access_token?&client_id="++ClientID++"&client_secret="++ClientSecret++"&code="++FBCode++"&redirect_uri="++FullRedirectUrl,
    ?LOG_DEBUG("request_facebook_access_token: requesting using URL - ~p", [Url]),

    % Request the page
    Resp=httpc:request(Url),
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
