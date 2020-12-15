<?php
class Dispatcher
{
    public function getController($request)
    {
        $url = trim(explode('?', $_SERVER['REQUEST_URI'])[0], '/');
        $parts = array_map('ucfirst', explode('/', $url));
        array_unshift($parts, 'Controller');

        $class = implode('_', $parts);

        while (!class_exists($class)) {
            array_pop($parts);
            if (empty($parts)) {
                throw new NotFoundException("Got 404 for $url");
            }
            $class = implode('_', $parts);
        }
        return new $class($request);
    }

    public function dispatch($request)
    {
        $controller = $this->getController($request);
        $action = $request->getAction();
        if (method_exists($controller, $action) && !in_array($action, ['login'])) {
            $controller->$action($request);
        } else {
            $controller->view($request);
        }
    }
}
