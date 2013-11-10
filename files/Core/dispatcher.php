<?php
class Dispatcher
{
    private $_preDispatch = array();
    private $_postDispatch = array();
    private $_controller;

    private function _getController($url)
    {
        $parts = array_map('ucfirst', explode('/', $url));
        array_unshift($parts, 'Controller');

        $file = FILES . DIRECTORY_SEPARATOR . implode(DIRECTORY_SEPARATOR, $parts) . '.php';
        $class = implode('_', $parts);

        while (!file_exists($file)) {
            array_pop($parts);
            if (empty($parts)) {
                $file = FILES . DIRECTORY_SEPARATOR . 'Controller' . DIRECTORY_SEPARATOR . 'Home.php';
                $class = 'Controller_Home';
                break;
            }
            $file = FILES . DIRECTORY_SEPARATOR . implode(DIRECTORY_SEPARATOR, $parts) . '.php';
            $class = implode('_', $parts);
        }
        include $file;

        try {
            if (!class_exists($class)) {
                throw new NotFoundRightException('Controller class "' . $class . '" not found');
            }

            $instance = new $class();

            if (!($instance instanceof Controller_Interface)) {
                throw new NotFoundRightException('Controller class "' . $controller . '" not instance of Controller_Interface');
            }
        } catch(ViewException $e) {
            Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
            ob_clean();
            Helper::get()->redirect('/error?id=' . $e->getErrorFile());
        } catch(Exception $e) {
            Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
            ob_clean();
            Helper::get()->redirect('/error?id=' . (new ViewException(''))->getErrorFile());
        }

        return $instance;
    }

    private function _findMethod($action)
    {
        $action  = str_replace('-', '_', $action);
        if (method_exists($this->_controller, $action)) {
            $method = $action;
        } else {
            $method = 'view';
        }
        return $method;
    }

    private function _runScript($filename)
    {
        try {
            include $filename;
        } catch(ViewException $e) {
            Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
            ob_clean();
            Helper::get()->redirect('/error?id=' . $e->getErrorFile());
        } catch(Exception $e) {
            Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
            ob_clean();
            Helper::get()->redirect('/error?id=' . (new ViewException(''))->getErrorFile());
        }
    }

    public function addPredispatchScript($filename)
    {
        $this->_preDispatch[] = $filename;
    }

    public function addPostDispatchScript($filename)
    {
        $this->_postDispatch[] = $filename;
    }

    function dispatch($url, $action, $id = null)
    {
        $this->_controller = $this->_getController($url);
        $method = $this->_findMethod($action);

        View::$controller = $this->_controller;

        foreach($this->_preDispatch as $file) {
            $this->_runScript($file);
        }

        try {
            $this->_controller->$method($id);
        } catch(ViewException $e) {
            Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
            ob_clean();
            Helper::get()->redirect('/error?id=' . $e->getErrorFile());
        } catch(Exception $e) {
            Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
            ob_clean();
            Helper::get()->redirect('/error?id=' . (new ViewException(''))->getErrorFile());
        }

        foreach($this->_postDispatch as $file) {
            $this->_runScript($file);
        }
    }
}
?>