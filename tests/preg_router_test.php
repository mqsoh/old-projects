<?php
require_once(dirname(__FILE__).'/../src/preg_router.php');

function preg_router_test_function_handler(){
    return func_get_args();
}

function global_call_count(){
    static $num = 0;
    $num++;
    return $num;
}

class preg_router_test extends PHPUnit_Framework_TestCase{
    public function instance_method_handler(){
        return func_get_args();
    }

    public static function class_method_handler(){
        return func_get_args();
    }

    public function catch_all(){
        return 'catch-all';
    }

    public static function call_count(){
        static $num = 0;
        $num++;
        return $num;
    }

    public function test_array_construction(){
        $router = new preg_router(array(
            array(
                '@^/function-handler/([^/\?]+)\?foo=([^&]+)@',
                "preg_router_test_function_handler"),
            array(
                '@^/instance-method-handler/([^/\?]+)\?foo=([^&]+)@',
                "preg_router_test::instance_method_handler"),
            array(
                '@^/class-method-handler/([^/\?]+)\?foo=([^&]+)@',
                "preg_router_test::class_method_handler"),
            array(
                '@.@',
                'preg_router_test::catch_all'),
        ));
        $this->assertEquals(get_class($router), 'preg_router');
        $this->behavior_test($router);
    }

    public function test_routes_ini(){
        $fn = tempnam(sys_get_temp_dir(), 'preg_router_test');
        $tmp = fopen($fn, 'w');
        fwrite($tmp, <<<EOF
[function handler]
pattern = "@^/function-handler/([^/\?]+)\?foo=([^&]+)@"
handler = preg_router_test_function_handler

[instance method handler]
pattern = "@^/instance-method-handler/([^/\?]+)\?foo=([^&]+)@"
handler = preg_router_test::instance_method_handler

[class method handler]
pattern = "@^/class-method-handler/([^/\?]+)\?foo=([^&]+)@"
handler = preg_router_test::class_method_handler

[catch all]
pattern = "@.@"
handler = preg_router_test::catch_all
EOF
        );
        fclose($tmp);

        $router = new preg_router($fn);
        $this->assertEquals(get_class($router), 'preg_router');
        $this->behavior_test($router);

        unlink($fn);
    }

    public function test_hooks(){
        $router = new preg_router(array(
            array(
                '@.*@',
                "preg_router_test_function_handler"),
        ));
        $router->before_all_handlers(array('preg_router_test', 'call_count'));
        $router->before_all_handlers('global_call_count');
        $router->after_all_handlers(function (){
            global_call_count();
        });
        $router->route('/');

        // It's called twice in the preg handlers and once when we test, so: 3.
        $this->assertEquals(global_call_count(), 3);

        // First from then handlers, then from the test, so: 2.
        $this->assertEquals(self::call_count(), 2);
    }

    /**
     * Tests router routing.
     */
    public function behavior_test($router){
        $uri = '/function-handler/foo?foo=bar';
        $this->assertEquals(
            $router->route($uri),
            array($uri, 'foo', 'bar'));

        $uri = '/instance-method-handler/foo?foo=bar';
        $this->assertEquals(
            $router->route($uri),
            array($uri, 'foo', 'bar'));

        $uri = '/class-method-handler/foo?foo=bar';
        $this->assertEquals(
            $router->route($uri),
            array($uri, 'foo', 'bar'));

        $uri = '/';
        $this->assertEquals(
            $router->route($uri),
            'catch-all');

        $uri = '/bunch/o/crap';
        $this->assertEquals(
            $router->route($uri),
            'catch-all');
    }
}
?>
