%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2011 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2011-12-16

%% @doc Support for the payment provider adyen.com

%% Copyright 2011 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_adyen).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("").
-mod_description("Support for the payment provider adyen.com").
-mod_prio(900).

-include_lib("include/zotonic.hrl").
-include_lib("../mod_shop/include/mod_shop.hrl").

-export([observe_get_payment_providers/3, event/2, adyen_offramp/2]).


observe_get_payment_providers(get_payment_providers, Acc, Context) ->
    [#payment_provider{name=?__("Adyen", Context),
                       module=?MODULE,
                       function=adyen_offramp}
     | Acc].



%% @doc Save the config values.
event({submit, {admin_save, []}, _, _}, Context) ->
    All = z_context:get_q([payment_page, merchant_account, skincode, secret, notification_username, notification_password], Context),
    [m_config:set_value(adyen, K, V, Context) || {K, V} <- All],
    z_render:wire({reload, []}, Context).


%% @doc Redirect to Adyen.
adyen_offramp(Order=#shop_order{}, Context) ->
    Url = adyen_support:payment_start(Order, Context),
    z_render:wire({redirect, [{location, Url}]}, Context).

