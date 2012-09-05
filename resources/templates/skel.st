skel(title, body_class, body) ::= <<
<!DOCTYPE html>
<html>
    <head>
        <title>{title} | Exocodex</title>
        <link
            rel="stylesheet" type="text/css"
            href="/public/bootstrap/css/bootstrap.min.css" />

        <script
            type="text/javascript"
            src="https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js"></script>
        <script
            type="text/javascript"
            src="/public/scripts.js"></script>
    </head>
    <body class="{body_class}">
        <div class="navbar">
            <div class="navbar-inner">
                <div class="container">
                    <a class="brand" href="/">
                        The Exocodex
                    </a>
                </div>
            </div>
        </div>

        <div class="container page">
            {body}
        </div>

        <div class="container footer">
            <a href="/">
                Home
            </a>
        </div>

        <script type="text/javascript">
            if (typeof(docready) !== 'undefined') {{
                docready(window);
            }}
        </script>
    </body>
</html>
>>
