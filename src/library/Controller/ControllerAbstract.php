<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Utility\Renderer;

abstract class ControllerAbstract implements ControllerInterface
{
    abstract public function view($id = null);

    public function sidebar()
    {
        $parent = get_parent_class($this);
        if ($parent !== false && $parent !== 'ControllerAbstract' && method_exists($parent, "sidebar")) {
            return call_user_method('sidebar', new $parent);
        }
        return '';
    }

    public function render($filename, array $vars = array(), $standalone = false)
    {
        $renderer = new Renderer();
        $content = $enderer->render($filename, $vars);

        if ($standalone) {
            echo $content;
            return;
        }
        include TISK ? HEADER_TISK : HEADER;
        echo $content;
        include TISK ? FOOTER_TISK : FOOTER;
    }
}