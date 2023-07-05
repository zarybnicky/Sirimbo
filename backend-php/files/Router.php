<?php
namespace Olymp;

class Router
{
    private array $routes;
    private string $baseNamespace;
    private string $currentPrefix;

    public function __construct(string $baseNamespace = '')
    {
        $this->routes = [];
        $this->baseNamespace = $baseNamespace == '' ? '' : $baseNamespace.'\\';
        $this->currentPrefix = '';
    }

    private function addRoute($method, $regex, $handler)
    {
        $this->routes[strtoupper($method)][$this->currentPrefix . $regex] = $handler;
    }

    public function get(string $regex, string $handler)
    {
        $this->addRoute('GET', $regex, $handler);
    }

    public function post(string $regex, string $handler)
    {
        $this->addRoute('POST', $regex, $handler);
    }

    public function redirect(string $regex, string $target)
    {
        $this->addRoute('GET', $regex, fn() => header("Location: $target"));
    }

    public function dispatch(string $method, string $path)
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
                return $this->call($callback, $params);
            }
        }
        throw new \NotFoundException('No route found');
    }

    private function call($callable, array $params = [])
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
