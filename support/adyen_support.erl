%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-29
%%
%% @doc Interface to the payment solution provider Adyen

%% https://ca-test.adyen.com/ca/ca/payments/searchPayments.shtml?query=8512411676185576&g=true&skipList=&search=Payment+Search

-module(adyen_support).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    install/1,
    periodic_log_check/1,
    payment_start/2,
    payment_completion/1,
    notification/2,
    handle_notification/2,
    test/0
]).

-define(SHOP_ORDER_SHIPMENT, ?WEEK).

-include_lib("zotonic.hrl").


install(Context) ->
    m_config:set_value(adyen, notification_username, "adyen", Context),
    m_config:set_value(adyen, notification_password, "", Context),
    m_config:set_value(adyen, payment_page, "https://test.adyen.com/hpp/pay.shtml", Context),
    m_config:set_value(adyen, merchant_account, "", Context),
    m_config:set_value(adyen, skincode, "", Context),
    m_config:set_value(adyen, secret, "", Context).


%% Handle log entries that were not handled, might be because of crashed between saving the entry and running the queries.
periodic_log_check(Context) ->
    LogIds = z_db:q("
        select id from shop_adyen_log
        where created < now() - interval '30 minutes'
          and handled = false
        limit 100
        ", Context),
    [ handle_notification(LogId, Context)  || {LogId} <- LogIds ].
    
    

%% @doc Build the payment uri to be used for paying the order.
%% @spec payment_uri(Id, Context) -> String
payment_start(Id, Context) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    Order = z_db:assoc_row("select * from shop_order where id = $1", [Id], Context),

    MerchantReference = integer_to_list(Id),
    PaymentAmount     = integer_to_list(proplists:get_value(total_price_incl, Order)),
    CurrencyCode      = "EUR",
    ShipBeforeDate    = erlydtl_dateformat:format(calendar:gregorian_seconds_to_datetime(NowSecs + ?SHOP_ORDER_SHIPMENT), "Y-m-d", Context),
    SkinCode          = m_config:get_value(adyen, skincode, Context),
    MerchantAccount   = m_config:get_value(adyen, merchant_account, Context),
    ShopperLocale     = case z_context:language(Context) of en -> "en_GB"; Lang -> atom_to_list(Lang) end,
    SessionValidity   = erlydtl_dateformat:format(calendar:universal_time_to_local_time(proplists:get_value(expires, Order)), "c", Context),
    ShopperEmail      = proplists:get_value(email, Order),
    ShopperReference  = z_convert:to_list(proplists:get_value(persistent_id, Order)),
    AllowedMethods    = "paypal,card,ideal",
    BlockedMethods    = "",
    OrderData         = "",
    Secret            = m_config:get_value(adyen, secret, Context),

    MerchantSig = sign(Secret, [PaymentAmount, CurrencyCode, ShipBeforeDate, MerchantReference, 
                                SkinCode, MerchantAccount, SessionValidity, ShopperEmail, ShopperReference, 
                                AllowedMethods, BlockedMethods]),

    Args = [
        % Signed
        {"paymentAmount", PaymentAmount},
        {"currencyCode", CurrencyCode},
        {"shipBeforeDate", ShipBeforeDate},
        {"merchantReference", MerchantReference},
        {"skinCode", SkinCode},
        {"merchantAccount", MerchantAccount},
        {"sessionValidity", SessionValidity},
        {"shopperEmail", ShopperEmail},
        {"shopperReference", ShopperReference},
        {"allowedMethods", AllowedMethods},
        {"blockedMethods", BlockedMethods},

        % Not signed
        {"shopperLocale", ShopperLocale},
        {"orderData", OrderData},
        {"merchantSig", MerchantSig}
    ],

    PaymentPage = z_convert:to_list(m_config:get_value(adyen, payment_page, Context)),
    PaymentPage ++ [$? | mochiweb_util:urlencode(Args)].



%% @doc Called when the payment completion is received, parameter is the context which contains all information.
%% @spec payment_completion(Context) -> {ok, OrderId} | {error, Reason}
payment_completion(Context) ->
    % http://127.0.0.1:8000/adyen/result?merchantReference=16&skinCode=OnCtxIfz&shopperLocale=nl&paymentMethod=ideal&authResult=AUTHORISED&pspReference=8612409656055305&merchantSig=8AjEyaKiP1xz%2Bp8Ef3BK50T%2Fu1Y%3D
    MerchantReference = z_context:get_q("merchantReference", Context, ""),
    SkinCode       = z_context:get_q("skinCode", Context, ""),
    _ShopperLocale = z_context:get_q("shopperLocale", Context, ""),
    PaymentMethod  = z_context:get_q("paymentMethod", Context, ""),
    AuthResult     = z_context:get_q("authResult", Context, ""),
    PspReference   = z_context:get_q("pspReference", Context, ""),
    MerchantSig    = z_context:get_q("merchantSig", Context),
    Secret         = m_config:get_value(adyen, secret, Context),

    CheckSig = try
        MerchantSig = binary_to_list(sign(Secret, [AuthResult, PspReference, MerchantReference, SkinCode]))
    catch
        error:{badmatch, _} -> {error, sig_invalid}
    end,
    
    case CheckSig of
        {error, Reason} -> 
            {error, Reason};
        _ ->
            % Signature valid, go ahead with this order.
            OrderId = list_to_integer(MerchantReference),
            shop_order:payment_completion(OrderId, auth_completion_result(AuthResult), PaymentMethod, PspReference, Context)
    end.

auth_completion_result("AUTHORISED") -> payment_authorized;
auth_completion_result("REFUSED") -> payment_refused;
auth_completion_result("PENDING") -> payment_pending;
auth_completion_result("ERROR") -> payment_error.


%% @doc Handle a notification sent by Adyen.  First saved in the database and then processed in another process.
notification(Qs, Context) ->
    case save_notification(Qs, Context) of
        {ok, LogId, _OrderId} ->
            % Start a separate process to handle this notification (and maybe older ones)
            spawn(?MODULE, handle_notification, [LogId, Context]),
            Context;
        {duplicate, _LogId, _OrderId} ->
            Context
    end.


%% @doc Handle a notification from Adyen, adapt the order status when needed (which might send some emails)
%% @spec handle_notification(LogId::integer(), #context) -> ok
handle_notification(LogId, Context) ->
    F = fun(Ctx) ->
        handle_notification_trans(LogId, Ctx),
        ok
    end,
    ok = z_db:transaction(F, Context).

handle_notification_trans(LogId, Context) ->
    {OrderId, EventCode, Success, PaymentMethod, PspReference} = z_db:q_row("
                select shop_order_id, event_code, success, payment_method, psp_reference
                from shop_adyen_log
                where id = $1", [LogId], Context),

    case OrderId of
        undefined -> 
            ok;
        _ ->
            NewState = case z_string:to_upper(EventCode) of
                "AUTHORISATION" ->
                    % When success = true then we got a payment
                    case Success of
                        true -> payment_authorized;
                        false -> payment_refused
                    end;

                "PENDING" -> payment_pending;

                "CAPTURE" -> % We received the money
                    skip;

                _ ->
                    % skip (cancellation, refund, dispute, report_available)
                    % We might want to warn the operator about cancellation, refund and dispute messages
                    skip
            end,

            case NewState of
                skip -> ok;
                _ -> shop_order:payment_completion(OrderId, NewState, PaymentMethod, PspReference, Context)
            end
    end,
    z_db:q("update shop_adyen_log set handled = true where id = $1", [LogId], Context),
    ok.
    

%% @doc Save the notification.  {ok, LogId, OrderId} when saved or {duplicate, LogId, OrderId}
save_notification(Qs, Context) ->
    OrderId = try z_convert:to_integer(proplists:get_value("merchantReference", Qs)) catch _:_ -> undefined end,
    Cols = [
        {shop_order_id, OrderId},
        {handled, false},
        {live, z_convert:to_bool(proplists:get_value("live", Qs))},
        {event_code, proplists:get_value("eventCode", Qs)},
        {psp_reference, proplists:get_value("pspReference", Qs)},
        {original_reference, proplists:get_value("originalReference", Qs)},
        {merchant_account_code, proplists:get_value("merchantAccountCode", Qs)},
        {event_date, proplists:get_value("eventDate", Qs)},
        {success, z_convert:to_bool(proplists:get_value("success", Qs))},
        {payment_method, proplists:get_value("paymentMethod", Qs)},
        {operations, proplists:get_value("operations", Qs)},
        {reason, proplists:get_value("reason", Qs)},
        {currency, proplists:get_value("currency", Qs)},
        {value, z_convert:to_integer(proplists:get_value("value", Qs))},
        {request, Qs}
    ],

    % Check for a duplicate
    Row = z_db:q_row("
                    select shop_order_id, max(id), max(success::integer)
                    from shop_adyen_log 
                    where event_code = $1 
                      and psp_reference = $2 
                    group by shop_order_id", 
                    [proplists:get_value(event_code, Cols),
                     proplists:get_value(psp_reference, Cols)], Context),

    case Row of
        undefined ->
            % new
            {ok, Id} = z_db:insert(shop_adyen_log, Cols, Context),
            {ok, Id, OrderId};
        {_Found, MaxId, MaxSuccess} ->
            % duplicate
            Success = proplists:get_value(success, Cols),
            case Success of
                true ->
                    case MaxSuccess of
                        1 ->
                            {duplicate, MaxId, OrderId};
                        0 ->
                            {ok, Id} = z_db:insert(shop_adyen_log, Cols, Context),
                            {ok, Id, OrderId}
                    end;
                false ->
                    % We might already have an success for this event.
                    {duplicate, MaxId, OrderId}
            end
    end.




%    DEBUG: resource_shop_adyen:50  [{"eventDate","2009-05-01T08:47:00.04Z"},
%                                    {"reason",[]},
%                                    {"originalReference",[]},
%                                    {"merchantReference","4"},
%                                    {"currency","EUR"},
%                                    {"pspReference","46598465465"},
%                                    {"merchantAccountCode","xxx"},
%                                    {"eventCode","AUTHORISATION"},
%                                    {"value","43475"},
%                                    {"operations","REFUND"},
%                                    {"success","true"},
%                                    {"paymentMethod","ideal"},
%                                    {"live","false"}]

% Notification api stati    :
% <input type="radio" name="eventCode"  value="AUTHORISATION" />AUTHORISATION<br />
% <input type="radio" name="eventCode"  value="CANCELLATION" />CANCELLATION<br />
% <input type="radio" name="eventCode"  value="PENDING" />PENDING<br />		
% <input type="radio" name="eventCode"  value="REFUND" />REFUND<br />
% <input type="radio" name="eventCode"  value="CAPTURE" />CAPTURE<br />
% <input type="radio" name="eventCode"  value="DISPUTE" />DISPUTE<br />
% <input type="radio" name="eventCode"  value="REPORT_AVAILABLE" />REPORT_AVAILABLE<br />
% <input type="radio" name="eventCode"  value="RECURRING_CONTRACT" />RECURRING_CONTRACT<br />
% <input type="radio" name="eventCode"  value="UKNOWN" />UKNOWN



sign(Secret, Data) ->
    base64:encode( crypto:sha_mac(Secret, Data) ).
    
test() ->
    <<"x58ZcRVL1H6y+XSeBGrySJ9ACVo=">> =
    sign("Kah942*$7sdp0)", "10000GBP2007-10-20Internet Order 123454aD37dJATestMerchant2007-10-11T11:00:00Z"),
    ok.
    
