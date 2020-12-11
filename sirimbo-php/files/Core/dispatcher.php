<?php
class Dispatcher
{
    public function getController($request)
    {
        $url = $request->getLiteralURI();
        $parts = array_map('ucfirst', explode('/', $url));
        array_unshift($parts, 'Controller');

        $class = implode('_', $parts);

        while (!class_exists($class)) {
            array_pop($parts);
            if (empty($parts)) {
                $class = 'Controller_Home';
                break;
            }
            $class = implode('_', $parts);
        }
        if (!class_exists($class)) {
            throw new NotFoundRightException('Class "' . $class . '" not found');
        }
        return new $class($request);
    }

    public function dispatch($request)
    {
        $controller = $this->getController($request);

        if (!($controller instanceof Controller_Interface)) {
            throw new NotFoundRightException(
                'Class "' . $controller . '" is not an instance of Controller_Interface'
            );
        }

        $action = $request->getAction();
        if (method_exists($controller, $action) && !in_array($action, ['login'])) {
            $controller->$action($request);
        } else {
            $controller->view($request);
        }
    }
}
