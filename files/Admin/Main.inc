<?php
User::checkPermissionsError(L_EDITOR);
header_main("Administrace");

interface Controller_Interface {
	function view($id = null);
}
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
			throw new Exception('Controller class "' . $controlle . '" not instance of ControllerInterface');
            
        $action  = str_replace('-', '_', $action);
		
		if(method_exists($controller, $action)) {
			$controller->$action($id);
		} else {
			$controller->view($id);
		}
	}	
}
Request::setURL(get('f'));
Request::setURI($_SERVER['REQUEST_URI']);

ob_start();
ob_start();

$d = new Dispatcher();
$d->dispatch(Request::getLiteralURL(), Request::getAction(), Request::getID());

$main = ob_get_clean();

include(TISK ? HEADER_TISK : HEADER);
echo $main;
include(TISK ? FOOTER_TISK : FOOTER);

ob_end_flush();
exit;
?>