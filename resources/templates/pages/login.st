login() ::= <<

<link
    rel="stylesheet" type="text/css"
    href="/public/openid/openid.css" />
<script
    type="text/javascript"
    src="/public/openid/openid-jquery.js"></script>
<script
    type="text/javascript"
    src="/public/openid/openid-en.js"></script>

<form action="/login-post" method="get" id="openid_form">
    <fieldset>
        <legend>Sign-in or Create New Account</legend>
        <div id="openid_choice">
            <p>Please click your account provider:</p>
            <div id="openid_btns"></div>
        </div>
        <div id="openid_input_area">
            <input id="openid-identity" name="openid-identity" type="text" value="http://" />
            <input id="openid_submit" type="submit" value="Sign-In" />
        </div>
        <noscript>
            <p>OpenID is service that allows you to log-on to many different websites using a single indentity.  Find out <a href="http://openid.net/what/">more about OpenID</a> and <a href="http://openid.net/get/">how to get an OpenID enabled account</a>.</p>
        </noscript>
    </fieldset>
</form>

<script type="text/javascript">
    openid.img_path = "/public/openid/";
    openid.init('openid-identity');
</script>

>>
