<?php
/*
$router = new AltoRouter();
$router->map('GET', '/', function() {
    require __DIR__ . '/views/home.php';
});
$router->map('GET', '/user/[i:id]/', function($id) {
    require __DIR__ . '/views/user-details.php';
});

$match = $router->match();
if (is_array($match) && is_callable( $match['target'])) {
    call_user_func_array($match['target'], $match['params']);
} else {
    header($_SERVER["SERVER_PROTOCOL"] . ' 404 Not Found');
}
*/
class Router
{
    protected $routes = array();
    protected $namedRoutes = array();
    protected $matchTypes = array(
        'i'  => '[0-9]++',
        'a'  => '[0-9A-Za-z]++',
        'h'  => '[0-9A-Fa-f]++',
        '*'  => '.+?',
        '**' => '.++',
        ''   => '[^/\.]++'
    );

    public function __construct($routes = array())
    {
        foreach ($routes as $route) {
            call_user_func_array(array($this, 'map'), $route);
        }
    }

    public function map($method, $route, $target, $name = null)
    {
        $this->routes[] = array($method, $route, $target, $name);

        if ($name) {
            if (isset($this->namedRoutes[$name])) {
                throw new \Exception("Can not redeclare route '{$name}'");
            } else {
                $this->namedRoutes[$name] = $route;
            }
        }
    }

    public function generate($routeName, array $params = array())
    {
        if (!isset($this->namedRoutes[$routeName])) {
            throw new \Exception("Route '{$routeName}' does not exist.");
        }

        $url = $this->namedRoutes[$routeName];

        if (preg_match_all('`(/|\.|)\[([^:\]]*+)(?::([^:\]]*+))?\](\?|)`', $url, $matches, PREG_SET_ORDER)) {
            foreach ($matches as $index => $match) {
                list($block, $pre, $type, $param, $optional) = $match;

                if ($pre) {
                    $block = substr($block, 1);
                }

                if (isset($params[$param])) {
                    $url = str_replace($block, $params[$param], $url);
                } elseif ($optional && $index !== 0) {
                    $url = str_replace($pre . $block, '', $url);
                } else {
                    $url = str_replace($block, '', $url);
                }
            }
        }
        return $url;
    }

    public function match($requestUrl = null, $requestMethod = null)
    {
        $params = array();
        $match = false;

        if ($requestUrl === null) {
            $requestUrl = isset($_SERVER['REQUEST_URI']) ? $_SERVER['REQUEST_URI'] : '/';
        }
        if (($strpos = strpos($requestUrl, '?')) !== false) {
            $requestUrl = substr($requestUrl, 0, $strpos);
        }
        if ($requestMethod === null) {
            $requestMethod = isset($_SERVER['REQUEST_METHOD']) ? $_SERVER['REQUEST_METHOD'] : 'GET';
        }

        foreach ($this->routes as $handler) {
            list($methods, $route, $target, $name) = $handler;

            if (!stripos($methods, $requestMethod) !== false) {
                continue;
            }

            if ($route === '*') {
                // * wildcard (matches all)
                $match = true;
            } elseif (isset($route[0]) && $route[0] === '@') {
                // @ regex delimiter
                $pattern = '`' . substr($route, 1) . '`u';
                $match = preg_match($pattern, $requestUrl, $params) === 1;
            } elseif (($position = strpos($route, '[')) === false) {
                // No params in url, do string comparison
                $match = strcasecmp($requestUrl, $route) === 0;
            } else {
                // Compare longest non-param string with url
                if (strncmp($requestUrl, $route, $position) !== 0) {
                    continue;
                }
                $regex = $this->compileRoute($route);
                $match = preg_match($regex, $requestUrl, $params) === 1;
            }

            if ($match) {
                if ($params) {
                    foreach (array_keys($params) as $key) {
                        if (is_numeric($key)) {
                            unset($params[$key]);
                        }
                    }
                }
                return array(
                    'target' => $target,
                    'params' => $params,
                    'name' => $name
                );
            }
        }
        return false;
    }

    protected function compileRoute($route)
    {
        if (preg_match_all('`(/|\.|)\[([^:\]]*+)(?::([^:\]]*+))?\](\?|)`', $route, $matches, PREG_SET_ORDER)) {
            $matchTypes = $this->matchTypes;
            foreach ($matches as $match) {
                list($block, $pre, $type, $param, $optional) = $match;

                if (isset($matchTypes[$type])) {
                    $type = $matchTypes[$type];
                }
                if ($pre === '.') {
                    $pre = '\.';
                }

                $optional = $optional !== '' ? '?' : null;
                $pattern = '(?:'
                        . ($pre !== '' ? $pre : null)
                        . '('
                        . ($param !== '' ? "?P<$param>" : null)
                        . $type
                        . ')'
                        . $optional
                        . ')'
                        . $optional;

                $route = str_replace($block, $pattern, $route);
            }
        }
        return "`^$route$`u";
    }
}
