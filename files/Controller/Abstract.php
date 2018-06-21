<?php
abstract class Controller_Abstract implements Controller_Interface
{
    abstract public function view($request);

    public function navbar() {
        $menu = include SETTINGS . '/menu/main.php';

        if (Permissions::check('nastenka', P_OWNED)) {
            $menu = array_merge($menu, include SETTINGS . '/menu/admin.php');
        }

        if (Permissions::check('nastenka', P_VIEW)) {
            $menu = array_merge($menu, include SETTINGS . '/menu/member.php');
        }

        return new Navbar(
            $menu,
            true
        );
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
            'navbar' => $this->navbar(),
            'meta' => [],
            'html_title' => ''
        ];
        if (isset($vars['sidebar'])) {
            $args['sidebar'] = $vars['sidebar'];
        }
        if (isset($vars['meta'])) {
            $args['meta'] = $vars['meta'];
        }
        if (isset($vars['html_title'])) {
            $args['html_title'] = $vars['html_title'];
        }
        echo $renderer->render(TISK ? HEADER_TISK : HEADER, $args);
        echo $content;
        echo $renderer->render(TISK ? FOOTER_TISK : FOOTER, ['filename' => $filename]);
    }

    public function __call($name, $args) {
        return Helper::invoke($name, $args);
    }
}
