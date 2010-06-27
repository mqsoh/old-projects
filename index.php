<?php

require_once('src/preg_router.php');
require_once('test/test-page.php');
$r = new preg_router('test/test-routes.ini');
$r->route($_SERVER['REQUEST_URI']);

?>
