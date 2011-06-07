-module(couch_util).
-compile(export_all).

get_value(Key, Proplist) ->
    get_value(Key, Proplist, undefined).
get_value(Key, Proplist, Default) ->
    proplists:get_value(Key, Proplist, Default).

json_decode("JsonStringFbGraphMeResponse") ->
    {
        [
            {<<"id">>, "AnIdFromFacebook"}
        ]
    }.

% json_apply_field and proplist_apply_field are copied
% directly from the real couch_util.erl
json_apply_field(H, {L}) ->
    json_apply_field(H, L, []).
json_apply_field({Key, NewValue}, [{Key, _OldVal} | Headers], Acc) ->
    json_apply_field({Key, NewValue}, Headers, Acc);
json_apply_field({Key, NewValue}, [{OtherKey, OtherVal} | Headers], Acc) ->
    json_apply_field({Key, NewValue}, Headers, [{OtherKey, OtherVal} | Acc]);
json_apply_field({Key, NewValue}, [], Acc) ->
    {[{Key, NewValue}|Acc]}.

proplist_apply_field(H, L) ->
    {R} = json_apply_field(H, {L}),
    R.

url_encode(Something) ->
    % we're not going to really encode it, just stamp it so
    % we can tell it was called.
    "url_encode("++Something++")".
