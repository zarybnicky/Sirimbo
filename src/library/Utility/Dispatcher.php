<?php
namespace TKOlomouc\Utility;

use TKOlomouc\Controller\ControllerInterface;
use TKOlomouc\View\Exception\NotFoundRightException;

class Dispatcher
{
    private $preDispatch = array();
    private $postDispatch = array();
    private $controller;

    private function getController($url)
    {
        $parts = array_map('ucfirst', explode('/', $url));
        array_unshift($parts, 'TKOlomouc', 'Controller');

        while (!class_exists(implode('\\', $parts))) {
            array_pop($parts);
            if (count($parts) == 2) {
                $parts[] = 'Home';
                break;
            }
        }

        $class = implode('\\', $parts);

        try {
            if (!class_exists($class)) {
                throw new NotFoundRightException('Controller class "' . $class . '" not found');
            }

            $instance = new $class();

            if (!($instance instanceof ControllerInterface)) {
                throw new NotFoundRightException('Controller class "' . $class . '" not instance of Controller_Interface');
            }
        } catch (ViewException $e) {
            Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
            ob_clean();
            Response::redirect('/error?id=' . $e->getErrorFile());
        } catch (Exception $e) {
            Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
            ob_clean();
            Response::redirect('/error?id=' . (new ViewException(''))->getErrorFile());
        }

        return $instance;
    }

    private function findMethod($action)
    {
        $action  = str_replace('-', '_', $action);
        if (method_exists($this->controller, $action)) {
            $method = $action;
        } else {
            $method = 'view';
        }
        return $method;
    }

    private function runScript($filename)
    {
        try {
            include $filename;
        } catch (ViewException $e) {
            Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
            ob_clean();
            Response::redirect('/error?id=' . $e->getErrorFile());
        } catch (Exception $e) {
            Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
            ob_clean();
            Response::redirect('/error?id=' . (new ViewException(''))->getErrorFile());
        }
    }

    public function addPredispatchScript($filename)
    {
        $this->preDispatch[] = $filename;
    }

    public function addPostDispatchScript($filename)
    {
        $this->postDispatch[] = $filename;
    }

    public function dispatch($url, $action, $id = null)
    {
        $this->controller = $this->getController($url);
        $method = $this->findMethod($action);

        View::$controller = $this->controller;

        foreach ($this->preDispatch as $file) {
            $this->runScript($file);
        }

        try {
            $this->controller->$method($id);
        } catch (ViewException $e) {
            Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
            ob_clean();
            Response::redirect('/error?id=' . $e->getErrorFile());
        } catch (Exception $e) {
            Log::write($e->getMessage() . ' (' . $e->getFile() . ':' . $e->getLine() . ")\n" . $e->getTraceAsString());
            ob_clean();
            Response::redirect('/error?id=' . (new ViewException(''))->getErrorFile());
        }

        foreach ($this->postDispatch as $file) {
            $this->runScript($file);
        }
    }
}
