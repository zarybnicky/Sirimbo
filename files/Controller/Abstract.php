<?php
abstract class Controller_Abstract implements Controller_Interface
{
    abstract public function view($id = null);

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

    public function render($filename, array $vars = array(), $standalone = false) {
        $renderer = new Renderer();
        $content = $renderer->render($filename, $vars);

        if ($standalone) {
            echo $content;
            return;
        }
        include TISK ? HEADER_TISK : HEADER;
        echo $content;
        include TISK ? FOOTER_TISK : FOOTER;
    }
    public function __call($name, $args) {
        $trace = debug_backtrace();
        $class = (isset($trace[1]['class']) ? $trace[1]['class'] : null);

        if (is_subclass_of($class, __CLASS__)) {
            if (empty($args))
                return Helper::get()->$name();
            else
                return call_user_func_array(array(Helper::get(), $name), $args);
        } else {
            throw new ViewException("Neplatná akce $name pro ${__CLASS__}");
        }
    }
}