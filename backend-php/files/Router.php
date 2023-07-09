<?php
namespace Olymp;

class Router
{
    private array $routes;
    private string $currentPrefix;

    public function __construct()
    {
        $this->routes = [];
        $this->currentPrefix = '';
    }

    public function addRoute($method, $regex, callable $handler)
    {
        $this->routes[strtoupper($method)][$this->currentPrefix . $regex] = $handler;
    }

    public function get(string $regex, callable $handler)
    {
        $this->addRoute('GET', $regex, $handler);
    }

    public function post(string $regex, callable $handler)
    {
        $this->addRoute('POST', $regex, $handler);
    }

    public function redirect(string $regex, string $target)
    {
        $this->addRoute('GET', $regex, fn() => header("Location: $target"));
    }

    public function dispatchGlobal()
    {
        $pos = strpos($_SERVER['REQUEST_URI'], '?');
        $method = $_SERVER['REQUEST_METHOD'];
        $path = '/' . trim(
            substr(
                $pos !== false ? substr($_SERVER['REQUEST_URI'], 0, $pos) : $_SERVER['REQUEST_URI'],
                strlen(implode('/', array_slice(explode('/', $_SERVER['SCRIPT_NAME']), 0, -1)) . '/')
            ),
            '/'
        );
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
                return call_user_func_array($callback, $params);
            }
        }
        throw new \NotFoundException('No route found');
    }
}
