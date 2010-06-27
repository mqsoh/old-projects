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
 *
 * ---------------------------------------------------------------------------
 *
 * This is a simple regex router. It uses a dosini file for route definitions.
 * Each entry should look like:
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
 * The handler uses a syntax that suggests that it's a static method but,
 * thanks to some PHP ambiguity, it can be a static or instance method.
 */
class preg_router{
    private $routes = NULL;
    private $pattern_map = array();

    /**
     * Constructs the router.
     *
     * @param string $file  The path to a dosini file to use for route definitions.
     */
    public function __construct($file = NULL){
        if (!$file){
            throw new preg_router_error('No routes file provided to the constructor.');
            return;
        }

        if (!is_file($file)){
            throw new preg_router_error('The routes file ('.$file.') is not a file.');
            return;
        }

        $f = parse_ini_file($file, true);
        $this->routes = $f;

        foreach ($this->routes as $name => $route){
            $this->pattern_map[$route['pattern']] = $name;
        }
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
                $route = $this->routes[$route_name];
                $arr = explode('::', $route['handler']);
                if (count($arr) != 2){
                    throw new preg_router_error("Malformed handler for route '$route_name'.");
                    return;
                }

                $class_name = $arr[0];
                $method_name = $arr[1];

                // Find class.
                if (!class_exists($class_name)){
                    throw new preg_router_error("Handler '$class_name' from route named '$route_name' doesn't exist.");
                    return;
                }
                $class = new ReflectionClass($class_name);
                $inst = $class->newInstance();

                // Find class's method.
                if (!$class->hasMethod($method_name)){
                    throw new preg_router_error("Method '$method_name' is not a method of '$class_name' from route named '$route_name'.");
                    return;
                }
                $method = $class->getMethod($method_name);

                // Invoke with the groups as arguments.
                // I'll always set $groups[0] to the $uri so that the first
                // argument to all handlers is the full request. $groups[0]
                // with query params seems to be truncated to the exact portion
                // of the URI matched, but the pattern may have been open-ended.
                $groups[0] = $uri;
                $method->invokeArgs($inst, $groups);
                return;
            }
        }
        throw new preg_router_error('No pattern matched.');
    }
}
class preg_router_error extends Exception{}
?>
