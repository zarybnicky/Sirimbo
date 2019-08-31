<?php
abstract class Controller_Abstract implements Controller_Interface
{
    abstract public function view($request);

    public function navbar()
    {
        return [include SETTINGS . '/menu/main.php'];
    }

    public function renderNavbarItem($item)
    {
        if (!is_array($item)) {
            return "<div class='$item'></div>";
        }
        if (isset($item[3])
            && $item[3]
            && !Permissions::check($item[3][0], $item[3][1])
        ) {
            return '';
        }
        if (!isset($item[2]) || empty($item[2])) {
            return '<li class="nav-item"><a class="nav-link" href="'
                . $item[1] . '">' . $item[0] . '</a></li>';
        }
        $x = '<a class="nav-link dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">' . $item[0] . '</a>';
        $x .= '<div class="dropdown-menu">';
        foreach ($item[2] as $sub) {
            if (isset($sub[3]) && $sub[3] && !Permissions::check($sub[3][0], $sub[3][1])) {
                continue;
            }
            $x .= '<a class="dropdown-item" href="' . $sub[1] . '">' . $sub[0] . '</a>';
        }
        $x .= '</div>';
        return '<li class="nav-item dropdown">' . $x . '</li>';
    }

    public function render($filename, array $vars = [], $standalone = false)
    {
        $renderer = new Renderer();
        $content = $renderer->render($filename, $vars);

        if ($standalone) {
            echo $content;
            return;
        }

        $args = [
            'renderNavbarItem' => [$this, 'renderNavbarItem'],
            'navbar' => $this->navbar(),
            'meta' => [],
            'content' => $content,
            'header' => isset($vars['header']) ? $vars['header'] : null,
            'subheader' => isset($vars['subheader']) ? $vars['subheader'] : null,
            'html_title' => ''
        ];
        if (isset($vars['meta'])) {
            $args['meta'] = array_map(
                function ($k, $v) {
                    return "$k=\"$v\"";
                },
                array_keys($vars['meta']),
                $vars['meta']
            );
        }
        if (isset($vars['html_title'])) {
            $args['html_title'] = $vars['html_title'];
        }

        echo $renderer->render(TEMPLATE, $args);
    }

    public function __call($name, $args)
    {
        return Helper::invoke($name, $args);
    }
}
