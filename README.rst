===========
preg_router
===========

This router makes no assumptions about anything. It doesn't get the request URI
for you; it doesn't tell you where to put your routes definition. I believe
that these aspects of development are trivial and require no abstraction. I
would rather define those conventions for my application.

I like INI files because they're simple and PHP has a native function
(parse_ini_file) that parses them quickly.

I think regular expressions are great and perfectly flexible.

Usage.
======

------------------
Route definitions.
------------------
Routes are defined when the preg_router is constructed. If you pass a string,
it assumes that it's a file name of an .ini file. You can also pass an array of
arrays.

INI file.
---------
Routes can be defined in a .ini file. INI files follow the same spec as the
php.ini file you're familiar with (loaded with PHP's native parse_ini_file).
The route entries for this router should look like::

    [route name]
    pattern = "@/some/(pattern)/(\d+)@"
    handler = "SomeClass::some_method"

Native array.
-------------
You can pass in an array of arrays. The first item in the nested array is
the pattern that gets passed to preg_match. The second item is the handler.
For example, you can pass this to the constructor::

    array(
        array('/foo/bar', 'Some::handler'),
        array('/bar/baz', 'Other::handler'),
    )

Handlers.
---------
Handlers can be functions, instance methods, or class methods. (See the
tests/preg_router_test.php for examples of all three.)

If the handler is delimited with '::', it's an instance or class method.  All
classes in the handler will be constructed (even if it's a class method; PHP
doesn't care if you call a static method on an instance of an object), but no
arguments are passed to the constructor.

All functions and methods will be passed a request object (see below). If any
groups are defined in the regex they will be passed as additional arguments to
the function. A route like this::

    [articles]
    pattern = "@/articles/meaningless-seo-friendly-title/(\d+)@"
    handler = "ArticlePage::display"

...can use a function definition like this::

    class ArticlePage{
        function display($req_object, $article_id){
            // $article_id is guaranteed to be provided and also an integer.
        }
    }

Hooks.
------
If you want to execute code before and/or after your handlers you can use
*before_all_handlers* and *after_all_handlers*. They use call_user_func on what
you pass in (with the same arguments passed to the handlers), so they can take
a string, Closure Object, or an array. Here are the examples from my unit
test::

    $router->before_all_handlers(array('preg_router_test', 'call_count'));
    $router->before_all_handlers('global_call_count');
    $router->after_all_handlers(function (){
        global_call_count();
    });

Request object.
---------------
I'd like to do functional tests on my request handlers. To this end I changed
the first argument of all handlers to use a *preg_router_request* object.

The request object has a uri property, which is the full URI of the matched
request. This object also has properties with copies of the PHP superglobals.

+-------------------------+-----------------+
| Request object property | PHP superglobal |
+=========================+=================+
| server                  | $_SERVER        |
+-------------------------+-----------------+
| get                     | $_GET           |
+-------------------------+-----------------+
| post                    | $_POST          |
+-------------------------+-----------------+
| files                   | $_FILES         |
+-------------------------+-----------------+
| cookie                  | $_COOKIE        |
+-------------------------+-----------------+
| session                 | $_SESSION       |
+-------------------------+-----------------+
| env                     | $_ENV           |
+-------------------------+-----------------+

Doing this provides an easier way of spoofing web requests in tests. If the
handlers use this request object as their sole means of access to the super
globals, you could ::

    class MyWebPage {
        public static function home($req) {
            if (!empty($req->post['foo'])) {
                echo $req->post['foo'];
            }
        }
    }
    class MyTest extends PHPUnit_Framework_TestCase {
        public function test_home_post() {
            $req = new preg_router_request('/');
            $req->get = array('foo' => 'A fake foo.');

            MyWebPage::home($req);
        }
    }

If a handler returns a non-empty value, the *preg_router* assumes it's a
request object and passes it on to the next function. This allows for chaining
when using the hooks and handlers. It also can serve as an application state;
if you were to augment it with a property, it'd be available to all later
functions. You might want to do some output buffer management, for instance.

It strikes me that this object isn't really a 'request'. I'm trying to think of
a better name.

Invoking.
=========

This class goes hand in hand with a front controller. In my Apache virtual host
definition, I have this::

    RewriteEngine On
    RewriteBase /
    RewriteCond %{REQUEST_FILENAME} !-fd
    RewriteRule . /index.php [L]

And in the docroot, I have an index.php that looks like this::

    require_once('src/preg_router.php');
    require_once('test/test-page.php');
    $r = new preg_router('test/test-routes.ini');
    $r->route($_SERVER['REQUEST_URI']);
