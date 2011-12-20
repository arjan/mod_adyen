%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011 Arjan Scherpenisse
%% @date 2011-12-20
%%
%% @doc Handles the return URL from adyen.

-module(resource_adyen_callback).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
         init/1,
         service_available/2, 
         moved_temporarily/2,
         resource_exists/2,
         previously_existed/2
]).


-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").
-include_lib("../../mod_shop/include/mod_shop.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.


service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:ensure_all(Context1),
    ?WM_REPLY(true, Context2).

resource_exists(ReqData, Context) ->
    {false, ReqData, Context}.

previously_existed(ReqData, Context) ->
    {true, ReqData, Context}.


moved_temporarily(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    PaymentDetails = z_context:get_q([merchantReference, skinCode, shopperLocale,
                                      paymentMethod, authResult, pspReference], Context1),

    Location = z_notifier:first(
                 #payment_status{
                    status=paid,
                    order_id=z_convert:to_integer(proplists:get_value(merchantReference, PaymentDetails)),
                    details=PaymentDetails},
                 Context1),
    ?WM_REPLY({true, Location}, Context1).


