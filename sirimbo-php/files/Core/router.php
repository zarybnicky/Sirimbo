<?php

class Router
{
    private array $routes;
    private $error;
    private string $baseNamespace;
    private string $currentPrefix;

    public function __construct(callable $error, string $baseNamespace = '')
    {
        $this->routes = [];
        $this->error = $error;
        $this->baseNamespace = $baseNamespace == '' ? '' : $baseNamespace.'\\';
        $this->currentPrefix = '';
    }

    public function route($method, $regex, $handler)
    {
        if ($method == '*') {
            $method = ['GET', 'PUT', 'DELETE', 'POST'];
        }

        foreach ((array)$method as $m) {
            $this->addRoute($m, $regex, $handler);
        }

        return $this;
    }

    private function addRoute($method, $regex, $handler)
    {
        $this->routes[strtoupper($method)][$this->currentPrefix . $regex] = $handler;
    }

    public function mount($prefix, callable $routes)
    {
        $previousPrefix = $this->currentPrefix;
        $this->currentPrefix = $previousPrefix . $prefix;
        $routes($this);
        $this->currentPrefix = $previousPrefix;
        return $this;
    }

    public function get(string $regex, string $handler)
    {
        $this->addRoute('GET', $regex, $handler);
        return $this;
    }

    public function post(string $regex, string $handler)
    {
        $this->addRoute('POST', $regex, $handler);
        return $this;
    }

    public function put(string $regex, string $handler)
    {
        $this->addRoute('PUT', $regex, $handler);
        return $this;
    }

    public function delete(string $regex, string $handler)
    {
        $this->addRoute('DELETE', $regex, $handler);
        return $this;
    }

    /**
     * Dispatches the router
     *
     * @param string $method The HTTP method, most likely from $_SERVER['REQUEST_METHOD']
     * @param string $path   The request path, most likely from some URL rewrite ?r=
     * @return mixed The router output
     */
    public function dispatch(string $method, string $path): mixed
    {
        foreach ($this->routes[$method] ?? [] as $regex => $callback) {
            $len = strlen($regex);
            if ($regex[0] != '/') {
                $regex = '/' . $regex;
            }
            if ($len > 1 && $regex[$len - 1] == '/') {
                $regex = substr($regex, 0, -1);
            }
            $regex = str_replace('@', '\\@', $regex);
            if (preg_match('@^' . $regex . '$@', $path, $params)) {
                array_shift($params);
                try {
                    return $this->call($callback, $params);
                } catch (HttpRequestException $ex) {
                    return $this->call($this->error, [$method, $path, $ex->getCode(), $ex]);
                } catch (Exception $ex) {
                    return $this->call($this->error, [$method, $path, 500, $ex]);
                }
            }
        }

        return $this->call($this->error, [$method, $path, 404, new HttpRequestException('No route found')]);
    }

    private function call(callable $callable, array $params = [])
    {
        if (is_string($callable)) {
            if ($callable[0] == '@') {
                $callable = $this->baseNamespace . substr($callable, 1);
            }
            $callable = str_replace('.', '\\', $callable);
        }
        if (is_array($callable)) {
            if ($callable[0][0] == '@') {
                $callable[0] = $this->baseNamespace . substr($callable[0], 1);
            }
            $callable[0] = str_replace('.', '\\', $callable[0]);
        }
        return call_user_func_array($callable, $params);
    }

    public function dispatchGlobal()
    {
        $pos = strpos($_SERVER['REQUEST_URI'], '?');
        return $this->dispatch($_SERVER['REQUEST_METHOD'], '/' . trim(
            substr(
                $pos !== false ? substr($_SERVER['REQUEST_URI'], 0, $pos) : $_SERVER['REQUEST_URI'],
                strlen(implode('/', array_slice(explode('/', $_SERVER['SCRIPT_NAME']), 0, -1)) . '/')
            ),
            '/'
        ));
    }
}

class HttpRequestException extends Exception
{
}
