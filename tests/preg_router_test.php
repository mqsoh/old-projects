<?php
require_once(dirname(__FILE__).'/../src/preg_router.php');

function preg_router_test_function_handler($req){
    $req->function_handler_args = array_slice(func_get_args(), 1);
    return $req;
}

function global_call_count($req){
    static $num = 0;
    $num += 1;
    $req->global_call_count = $num;
    return $req;
}

function ignore_req($req) {
    // pass
}

class preg_router_test extends PHPUnit_Framework_TestCase{
    public function instance_method_handler($req){
        $req->instance_method_args = array_slice(func_get_args(), 1);
        return $req;
    }

    public static function class_method_handler($req){
        $req->class_method_args = array_slice(func_get_args(), 1);
        return $req;
    }

    public function catch_all($req){
        $req->catch_all = 'catch-all';
        return $req;
    }

    public static function call_count($req){
        static $num = 0;
        $num += 1;
        $req->call_count = $num;
        return $req;
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
        $router->before_all_handlers('ignore_req');
        $router->before_all_handlers('global_call_count');
        $router->after_all_handlers(function ($req){
            global_call_count($req);});
        $router->after_all_handlers('ignore_req');
        $resp = $router->route('/');

        $this->assertEquals($resp->global_call_count, 2);
        $this->assertEquals($resp->call_count, 1);
    }

    /**
     * Tests router routing.
     */
    public function behavior_test($router){
        $uri = '/function-handler/foo?foo=bar';
        $resp = $router->route($uri);
        $this->assertEquals(
            $resp->function_handler_args,
            array('foo', 'bar'));

        $uri = '/instance-method-handler/foo?foo=bar';
        $resp = $router->route($uri);
        $this->assertEquals(
            $resp->instance_method_args,
            array('foo', 'bar'));

        $uri = '/class-method-handler/foo?foo=bar';
        $resp = $router->route($uri);
        $this->assertEquals(
            $resp->class_method_args,
            array('foo', 'bar'));

        $uri = '/';
        $resp = $router->route($uri);
        $this->assertEquals(
            $resp->catch_all,
            'catch-all');

        $uri = '/bunch/o/crap';
        $resp = $router->route($uri);
        $this->assertEquals(
            $resp->catch_all,
            'catch-all');
    }
}
?>
