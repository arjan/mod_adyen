{% if m.acl.use.mod_adyen %}
<li><a href="{% url admin_adyen %}" {% if selected == 'adyen' %}class="current"{% endif %} title=_"Adyen configuration">Adyen</a></li>
{% endif %}
