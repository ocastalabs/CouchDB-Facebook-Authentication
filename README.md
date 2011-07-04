Facebook Authentication for CouchDB
===================================

CouchDB has a number of Authentication modules built in (cookie, default & OAuth) and several open source projects for others such as LDAP and
OpenID. This project adds a Facebook authentication module that uses the
[Facebook authentication API](http://developers.Facebook.com/docs/authentication/ ) to log people in.

It consists of a CouchDB httpd\_global\_handler that responds to http GET requests and
initiates the Facebook login sequence. Once logged in, the handler requests an access
code and retrieves the Facebook profile information via https://graph.Facebook.com/me. The user's Facebook ID can
then be used to retrieve the CouchDB users document from the authentication_db to create the CouchDB session. The access code is stored
in the user record. If there isn't a matching user record then one is created using the Facebook ID as the username

An error during Facebook login will return HTTP 403 to the original caller. 

Example Flow of Control
---------------------------
 
For our example we assume the app is hosted at my_site.com

1. The user clicks the link my_site.com/_fb
2. The _\_fb_ page redirects user to Facebook so user can approve the apps access to their details
3. The user logs into Facebook (if not already logged in) and approves the app (if
not already approved). NOTE: If the user was already logged in and has already approved the app then
they will be redirected back to your app without needing to type anything.
4. The user arrives back at the _\_fb\_resp_ page with a code that allows this module
to contact Facebook via the API and request an Access Code. No user interaction
required.
5. Once the Access Code is granted it allows the module to retrieve the users id
(used as the couch users username when creating the couch user), and is stored
in the users record so that it can be used by the app.
6. When the couch session expires (the app will have to catch this) you simply
need to send the user to _\_fb_ again and, because they've already gone thru this
route, it should get you all the way back to _\_fb\_resp_ without any user
interaction needed.
7. The final act is to redirect the user back into the app, a location that is
a combination of _client\_app\_uri_ config directive and the _clientapptoken_
param to _\_fb_.


Build
--------------------

There is an unsophisticated Makefile with targets for _test_ (requires eunit), _compile_ and _install_. 

In order to compile and install this module you might have to edit the Makefile and change one or more of _COUCH\_ROOT_, _\_COUCHDB\_ERLANG\_LIB_, _COUCHDB\_LOCALD_ and _COUCHDB\_INIT\_SCRIPT_ values to point to the appropriate directories and file within your couchdb installation.


Installation
-------------------

You need to copy the beam file to somewhere where couch can find it. That location could be something like couchdb/erlang/lib/couch-1.0.1/ebin/ it depends on where/how you've installed couch.

You'll also need to create a [Facebook app](See http://developer.Facebook.com)

Configuration
--------------------
You'll need to add an ini file or ini entries in couch config to use this module.

          [httpd_global_handlers]
          _fb = {fb_auth, handle_fb_req}
          _fb_resp = {fb_auth, handle_fb_resp_req}
 
          [fb]
          scope=offline_access,user_about_me
          client_id=1234567890
          redirect_uri=http://my_awesome_app.com/_fb_resp
          client_secret=1234567890ABCDEF123456789
          destination_db=_users
          client_app_uri=http://my_awesome_app.com/home?


**\_fb**  
  This is the couch location for the code that redirects the user to Facebook.
  Pass in a param called _clientapptoken_ if you want something added to the
  client app redirect at the end of the auth process (for when control is
  returned to your app).

**\_fb\_resp**  
  This is the couch location that Facebook should redirect back to. This should
  match the _redirect\_uri_ field in the [fb] section.

  NOTE: Facebook requires that your site is public. The 'Site URL' setting of
        the Facebook app needs to be set to your site.

**scope** 
  A comma separated list of the scope of access required to Facebook by the app.
  See Facebook dev docs for all the scope names. At least _offline\_access_ and
  _user\_about\_me_ are required.

**client_id**  
  The App ID of your Facebook app

**redirect_uri**  
  This is the location that Facebook will be told to return the user to when
  they're done logging in. This MUST start with the same location that you set
  for Site URL in the Facebook app

**client\_secret**  
  DO NOT PUBLISH THIS! It is the _App Secret_ from your Facebook app.
  It is used behind the scenes to contact Facebook. If anyone gets hold
  of this they can pretend to be your app. Beware!

**destination\_db**  
  This is the name of the database that the user record will be created in.
  This is probably _\_users_ , but is configurable in case you want to test
  the app without risking your existing _users database.

**client\_app\_uri**  
  When _\_fb\_resp_ has been reached it will then need to return the flow to
  your app. You control where this redirect location with this setting.
  Any value passed to the initial _\_fb__ call param _clientapptoken_ will be
  appended to this URL.

  Licenses
---------------

  CouchDB-Facebook Authentication is licensed under: Apache License Version 2.0, January 2004 http://www.apache.org/licenses/

  Copyright (c) 2011 Ocasta Labs Ltd.

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

