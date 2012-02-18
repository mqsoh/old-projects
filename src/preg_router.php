<?php
/**
 * Copyright © 2010 Mason Staugler <http://www.staugler.net>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/**
 * This is a regex router. It uses a dosini file for route definitions or an
 * array of arrays (passed to the constructor).
 *
 * Native array spec.
 * ==================
 * You can pass in an array of arrays. The first item in the nested array is
 * the pattern that gets passed to preg_match. The second item is the handler.
 * For example::
 *
 *     array(
 *         array('/foo/bar', 'Some::handler'),
 *         array('/baz/buzz', 'Other::handler'),
 *     )
 *
 * Ini file spec.
 * ==============
 * Each entry should look like::
 *
 *     [route name]
 *     pattern = "@/some/(pattern)/(\d+)@"
 *     handler = "SomeClass::some_method"
 *
 * Route name.
 * -----------
 * The route name must be unique.
 *
 * Pattern.
 * --------
 * The pattern gets fed into preg_match.
 *
 * Handler.
 * --------
 * The handler is a function, instance method, or class method. The '::'
 * denotes a method.
 */
class preg_router{
    private $routes = NULL;
    private $pattern_map = array();
    private $prehandler_queue = array();
    private $posthandler_queue = array();

    /**
     * Constructs the router.
     *
     * @param * $routes
     *     If $routes is a string, it's the path to a dosini file to use for
     *     route definitions. If it's an array, it's an array with nested
     *     arrays. See the class documentation for the spec.
     */
    public function __construct($routes = NULL){
        if (empty($routes)){
            throw new preg_router_error('No routes file provided to the constructor.');
            return;
        }

        if (is_string($routes)){
            if (!is_file($routes)){
                throw new preg_router_error('The routes file ('.$routes.') is not a file.');
                return;
            }

            $f = parse_ini_file($routes, true);
            $this->routes = $f;

            foreach ($this->routes as $name => $route){
                $this->pattern_map[$route['pattern']] = $name;
            }
        }
        else if (is_array($routes)){
            foreach ($routes as $ii => $route){
                if (count($route) != 2){
                    throw new preg_router_error("Route number $ii has should be an array of two items: the pattern and the handler.");
                }
                $this->routes[$ii] = array('pattern' => $route[0], 'handler' => $route[1]);
                $this->pattern_map[$route[0]] = $ii;
            }
        }
        else{
            throw new preg_router_error('Invalid routes passed to the constructor.');
        }
    }

    /**
     * Add a function to be executed before all handlers. It's a queue.
     *
     * @param mixed $fun
     *     If $fun is a string, call_user_function will be used. Otherwise,
     *     I'll assume that it's an anonymous function.
     */
    public function before_all_handlers($fun){
        $this->prehandler_queue[] = $fun;
    }

    /**
     * Add a function to be executed after all handlers. It's a queue.
     *
     * @param mixed $fun
     *     If $fun is a string, call_user_function will be used. Otherwise,
     *     I'll assume that it's an anonymous function.
     */
    public function after_all_handlers($fun){
        $this->posthandler_queue[] = $fun;
    }

    /**
     * You'll probably want to pass in $_SERVER['REQUEST_URI'] for the URI,
     * but it can be anything if, for instance, you wanted to use some sort of
     * internal addressing.
     *
     * This will match a pattern and execute a handler defined in the dosini
     * file passed into the constructor.
     *
     * @param string $uri  The URI to match.
     */
    public function route($uri){
        foreach ($this->pattern_map as $pat => $route_name){
            if (preg_match($pat, $uri, $groups)){
                // I'll always set $groups[0] to the $uri so that the first
                // argument to all handlers is the full request. $groups[0]
                // with query params seems to be truncated to the exact portion
                // of the URI matched, but the pattern may have been
                // open-ended.
                $groups[0] = new preg_router_request($uri);

                $prehandler_response = $this->prehandlers($groups);
                if ($prehandler_response) {
                    $groups[0] = $prehandler_response;
                }

                $route = $this->routes[$route_name];
                $arr = explode('::', $route['handler']);
                $pieces = count($arr);

                if ($pieces < 1){
                    throw new preg_router_error("Empty handler for route '$route_name'.");
                }
                if ($pieces == 1){
                    // Handler is a function.
                    $function_name = $arr[0];
                    if (!function_exists($function_name)){
                        throw new preg_router_error("No function '$function_name' exists.");
                    }
                    $handler_resp = call_user_func_array($function_name, $groups);
                    if ($handler_resp) {
                        $groups[0] = $handler_resp;
                    }

                    return $this->posthandlers($groups);
                }
                else if ($pieces == 2){
                    // Handler is a method.
                    $class_name = $arr[0];
                    $method_name = $arr[1];
                    $handler_resp = $this->handle_method($class_name, $method_name, $groups);
                    if ($handler_resp) {
                        $groups[0] = $handler_resp;
                    }

                    return $this->posthandlers($groups);
                }
                else{
                    throw new preg_router_error("Malformed handler for route '$route_name'.");
                }
            }
        }
        throw new preg_router_error('No pattern matched.');
    }

    /**
     * A small abstraction to make 'route' clear.
     *
     * @param string $class_name
     * @param string $method_name
     * @param array $args
     *
     * @return
     *     The result of executing 'method_name' on an instance of
     *     'class_name' with the 'args' as arguments.
     */
    private function handle_method($class_name, $method_name, $args){
        // Find class.
        if (!class_exists($class_name)){
            throw new preg_router_error("Handler '$class_name' doesn't exist.");
            return;
        }
        $class = new ReflectionClass($class_name);
        $inst = $class->newInstance();

        // Find class's method.
        if (!$class->hasMethod($method_name)){
            throw new preg_router_error("Method '$method_name' is not a method of '$class_name'.");
            return;
        }
        $method = $class->getMethod($method_name);

        // Invoke with the groups as arguments.
        return $method->invokeArgs($inst, $args);
    }

    /**
     * Executes any functions added to the 'before' queue.
     */
    private function prehandlers($args){
        foreach ($this->prehandler_queue as $fun){
            $resp = call_user_func_array($fun, $args);
            if ($resp) {
                $args[0] = $resp;
            }
        }

        return $args[0];
    }

    /**
     * Executes any functions added to the 'after' queue.
     */
    private function posthandlers($args){
        foreach ($this->posthandler_queue as $fun){
            $resp = call_user_func_array($fun, $args);
            if ($resp) {
                $args[0] = $resp;
            }
        }

        return $args[0];
    }
}

/**
 * A container for router requests. Prehandlers, handlers, and posthandlers
 * will be passed one of these. They may also return one of these and any
 * subsequent function call will receive the updated copy. I'm providing this
 * to allow for functional testing of pure function request handlers. You can
 * spoof any aspect of a web request.
 */
class preg_router_request {
    public $uri = null;
    public $server = null;
    public $get = null;
    public $post = null;
    public $files = null;
    public $cookie = null;
    public $session = null;
    public $env = null;

    /**
     * The local copies of the superglobals will be initialized to the current
     * state of the superglobal if it isn't empty.
     */
    public function __construct($uri = null) {
        $this->uri = $uri;

        if (!empty($_SERVER)) {
            $this->server = $_SERVER;
        }

        if (!empty($_GET)) {
            $this->get = $_GET;
        }

        if (!empty($_POST)) {
            $this->post = $_POST;
        }

        if (!empty($_FILES)) {
            $this->files = $_FILES;
        }

        if (!empty($_COOKIE)) {
            $this->cookie = $_COOKIE;
        }

        if (!empty($_SESSION)) {
            $this->session = $_SESSION;
        }

        if (!empty($_ENV)) {
            $this->env = $_ENV;
        }
    }
}

/**
 * Any kind of error with the preg_router. It's the message that's significant.
 */
class preg_router_error extends Exception{}
?>
