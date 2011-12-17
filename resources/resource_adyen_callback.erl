%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-05-01
%%
%% @doc Handles notifications received from the payment service
%% provider Adyen.  The notifications are sent using a POST.

-module(resource_adyen_callback).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    dispatch/0,
    init/1, 
    is_authorized/2,
    allowed_methods/2,
    content_types_provided/2,
    process_post/2
]).


-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").

-define(AUTH_HEAD, "Basic realm=AdyenNotification").

dispatch() ->
    [
        {shop_adyen_notification,  ["shop","adyen","notification"], ?MODULE, []}
    ].


init([]) -> {ok, []}.


%% Check the HTTP basic authentication supplied by Adyen
is_authorized(ReqData, _Context) ->
    Context1 = z_context:new(ReqData, ?MODULE),
    Context2 = z_context:ensure_qs(Context1),
    case wrq:get_req_header("Authorization", ReqData) of
        "Basic " ++ Base64 ->
            Username = z_convert:to_list(m_config:get_value(adyen, notification_username, "adyen", Context2)),
            Password = z_convert:to_list(m_config:get_value(adyen, notification_password, "plop!", Context2)),
            case string:tokens(base64:mime_decode_to_string(string:strip(Base64)), ":") of
                [Username, Password] -> 
                    ?WM_REPLY(true, Context2);
                _ ->
                    ?LOG("Adyen notification: wrong username and/or password.", []),
                    ?WM_REPLY(?AUTH_HEAD, Context2)
            end;
        _ ->
            ?WM_REPLY(?AUTH_HEAD, Context2)
    end.
   

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) -> 
    %% When handling a POST the content type function is not used, so supply false for the function.
    { [{"text/plain", false}], ReqData, Context }.

process_post(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),

    % Handle the notification, save it in the database
    shop_adyen:notification(z_context:get_q_all(Context1), Context1),

    RD  = z_context:get_reqdata(Context1),
    RD1 = wrq:append_to_resp_body(<<"[accepted]">>, RD),
    ReplyContext = z_context:set_reqdata(RD1, Context1),
    ?WM_REPLY(true, ReplyContext).


