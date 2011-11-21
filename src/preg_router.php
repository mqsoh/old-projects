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
 * This is a simple regex router. It uses a dosini file for route definitions
 * or an array of arrays (passed to the constructor).
 *
 * Native array spec.
 * ==================
 * You can pass in an array of arrays. The first item in the nested array is
 * the pattern that gets passed to preg_match. The second item is the handler.
 * For example::
 *
 *     array(
 *         array('/foo/bar', 'Some::handler'),
 *         array('/bar/baz', 'Other::handler'),
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
     *     If $routs is a string, it's the path to a dosini file to use for
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
                $groups[0] = $uri;

                $this->prehandlers($groups);

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
                    $resp = call_user_func_array($function_name, $groups);

                    $this->posthandlers($groups);
                    return $resp;
                }
                else if ($pieces == 2){
                    // Handler is a method.
                    $class_name = $arr[0];
                    $method_name = $arr[1];
                    $resp = $this->handle_method($class_name, $method_name, $groups);

                    $this->posthandlers($groups);
                    return $resp;
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
            call_user_func($fun, $args);
        }
    }

    /**
     * Executes any functions added to the 'after' queue.
     */
    private function posthandlers($args){
        foreach ($this->posthandler_queue as $fun){
            call_user_func($fun, $args);
        }
    }
}
class preg_router_error extends Exception{}
?>
