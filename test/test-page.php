<?php
class TestPage{
	public static function class_method($uri){
		echo "Function arguments.\n-------------------\n".print_r(func_get_args(), true);
		echo "\n";
		echo "_GET.\n-----\n".print_r($_GET, true);
		echo "\n";
		echo "_POST.\n------\n".print_r($_POST, true);
	}

	public function instance_method($uri){
		echo "Function arguments.\n-------------------\n".print_r(func_get_args(), true);
		echo "\n";
		echo "_GET.\n-----\n".print_r($_GET, true);
		echo "\n";
		echo "_POST.\n------\n".print_r($_POST, true);
	}

	public function complicated($uri, $one, $two, $three){
		echo "uri: $uri\none: $one\ntwo: $two\nthree: $three\n\n";

		echo "Function arguments.\n-------------------\n".print_r(func_get_args(), true);
		echo "\n";
		echo "_GET.\n-----\n".print_r($_GET, true);
		echo "\n";
		echo "_POST.\n------\n".print_r($_POST, true);
	}

	public function four_oh_four($uri){
		echo "404 handler";
	}
}
?>
