{% extends "admin_base.tpl" %}

{% block title %}{_ Adyen configuration _}{% endblock %}

{% block content %}

<div id="content" class="zp-85">
    <div class="block clearfix">
        
	<h2>{_ Adyen configuration _}</h2>
	<p>{_ Here you find settings to optimize the indexing of your site by search engines. _}</p>

	{% wire id="form" type="submit" postback={admin_save} delegate="mod_adyen" %}
	<form name="form" id="form" method="POST" action="postback">

	    <div class="item-wrapper">
	        <h3 class="above-item">{_ Adyen skin _}</h3>
	        <div class="item">
		    <fieldset class="admin-form">
		        <div class="form-item clearfix">
			    <label for="payment_page">{_ Payment page _}</label>
			    <input type="text" id="payment_page" name="payment_page" value="{{ m.config.adyen.payment_page.value|escape }}" />
		        </div>
		        <div class="form-item clearfix">
			    <label for="merchant_account">{_ Merchant account _}</label>
			    <input type="text" id="merchant_account" name="merchant_account" value="{{ m.config.adyen.merchant_account.value|escape }}" />
		        </div>
		        <div class="form-item clearfix">
			    <label for="skincode">{_ Skin code _}</label>
			    <input type="text" id="skincode" name="skincode" value="{{ m.config.adyen.skincode.value|escape }}" />
		        </div>
		        <div class="form-item clearfix">
			    <label for="secret">{_ Secret _}</label>
			    <input type="text" id="secret" name="secret" value="{{ m.config.adyen.secret.value|escape }}" />
		        </div>
                    </fieldset>
                </div>
	    </div>

	    <div class="item-wrapper">
	        <h3 class="above-item">{_ Adyen notification _}</h3>
	        <div class="item">
		    <fieldset class="admin-form">
		        <div class="form-item clearfix">
			    <label for="notification-username">{_ Username _}</label>
			    <input type="text" id="notification-username" name="notification_username" value="{{ m.config.adyen.notification_username.value|escape }}" />
		        </div>
		        <div class="form-item clearfix">
			    <label for="notification-password">{_ Password _}</label>
			    <input type="text" id="notification-password" name="notification_password" value="{{ m.config.adyen.notification_password.value|escape }}" />
		        </div>
                    </fieldset>
                </div>
	    </div>
            
	    <div class="form-item clearfix">
	        <button type="submit">{_ Save Adyen settings _}</button>
	    </div>
        </form>        
    </div>
</div>
{% endblock %}
