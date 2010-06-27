===========
preg_router
===========

This router makes no assumptions about anything. It doesn't get the request URI for you; it doesn't tell you where to put your routes definition. I believe that these aspects of development are trivial and require no abstraction. I would rather define those conventions for my application.

I like INI files because they're simple and PHP has a native function (parse_ini_file) that parses them quickly.

I think regular expressions are great and perfectly flexible.

Usage.
======

Route definitions.
------------------
Route definitions go into a dosini file (routes.ini). INI files follow the same spec as the php.ini file you're familiar with. The route entries for this router should look like::

    [route name]
    pattern = "@/some/(pattern)/(\d+)@"
    handler = "SomeClass::some_method"

Handlers.
---------
The handler in the route definitions uses ::, which looks like a static method but, because of some aspects of PHP that I've never investigated very deeply, it's completely ambiguous whether or not that method is a static or instance method.

All classes in the handler will be constructed, but no arguments are passed to the constructor. All methods will be passed the URI that the router used to match. If any groups are defined in the regex they will be passed as additional arguments to the function. A route like this::

    [articles]
    pattern = "@/articles/meaningless-seo-friendly-title/(\d+)@"
    handler = "ArticlePage::display"

...can use a function definition like this::

    class ArticlePage{
        function display($uri, $article_id){
            // $article_id is guaranteed to be provided and also an integer.
        }
    }

Invoking.
=========

This class goes hand in hand with a front controller. In my Apache virtual host definition, I have this::

    RewriteEngine On
    RewriteBase /
    RewriteCond %{REQUEST_FILENAME} !-f
    RewriteCond %{REQUEST_FILENAME} !-d
    RewriteRule . /index.php [L]

And in the docroot, I have an index.php that looks like this::

    require_once('src/preg_router.php');
    require_once('test/test-page.php');
    $r = new preg_router('test/test-routes.ini');
    $r->route($_SERVER['REQUEST_URI']);
