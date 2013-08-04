<?php
class Dispatcher {
	function getController($url) {
		$parts = array_map('ucfirst', explode('/', $url));
		array_unshift($parts, 'Controller');

		$file = FILES . DIRECTORY_SEPARATOR . implode(DIRECTORY_SEPARATOR, $parts) . '.php';
		$class = implode('_', $parts);

		while(!file_exists($file)) {
			array_pop($parts);
			if(empty($parts)) {
				$file = FILES . DIRECTORY_SEPARATOR . 'Controller' . DIRECTORY_SEPARATOR . 'Home.php';
				$class = 'Controller_Home';
				break;
			}
			$file = FILES . DIRECTORY_SEPARATOR . implode(DIRECTORY_SEPARATOR, $parts) . '.php';
			$class = implode('_', $parts);
		}
		require($file);
		if(class_exists($class))
			return new $class();
		else
			throw new Exception('Controller class "' . $class . '" not found');
	}
	function dispatch($url, $action, $id = null) {
		$controller = $this->getController($url);

		if(!($controller instanceof Controller_Interface))
			throw new Exception('Controller class "' . $controller . '" not instance of Controller_Interface');

		View::$controller = $controller;
		
		$action  = str_replace('-', '_', $action);

		if(method_exists($controller, $action)) {
			$controller->$action($id);
		} else {
			$controller->view($id);
		}
	}
}
?>