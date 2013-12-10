<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\Model\Database\Adapter;
use TKOlomouc\Model\DBAnkety;
use TKOlomouc\View\Partial;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\Request;

class Sidebar extends Partial
{
    private $request;
    private $hasSidebarDiv = false;

    public function __construct(Request $request)
    {
        $this->request = $request;
    }

    protected function prefix()
    {
        if (!$this->hasSidebarDiv) {
            return '<style type="text/css">#content{margin-left:110px !important;}</style>';
        }
        $this->hasSidebarDiv = true;
    }

    public function menuHeader()
    {
        return $this->prefix()
            . $this->blackBox('<span class="logo"></span>Menu');
    }

    public function menuItem($text, $link, $module = '', $permission = L_ALL)
    {
        if ($module != '' && !Permissions::check($module, $permission)) {
            return;
        }
        $label = '<span class="arrow">.</span>' . $text;

        if (stripos($this->request->getURL(), $link) === 0) {
            return $this->prefix()
            . $this->blackBox($label . '<span class="point">.</span>', 'sidebar current', $link);
        } else {
            return $this->prefix()
            . $this->whiteBox($label, 'sidebar', $link);
        }
    }

    public function blackBox($text, $class = '', $link = '')
    {
        $text = '<div class="dark-in">' . $text . '</div>';

        if ($link != '') {
            $text = '<a href="' . $link . '">' . $text . '</a>';
        }
        return $this->prefix()
            . '<div class="dark-out'
            . ($class ? ' ' . $class : '') . '">' . $text . '</div>';
    }

    public function whiteBox($text, $class = '', $link = '')
    {
        $text = '<div class="light-in">' . $text . '</div>';

        if ($link != '') {
            $text = '<a href="' . $link . '">' . $text . '</a>';
        }
        return $this->prefix()
            . '<div class="light-out'
            . ($class ? ' ' . $class : '') . '">' . $text . '</div>';
    }

    public function commonItems()
    {

        $out = $this->prefix();

        if (!Adapter::isDatabaseError($this->request)) {
            $ankety = (new \TKOlomouc\View\Main\Ankety\Sidebar())->render();

            if ($ankety) {
                $out .= $this->blackBox('Ankety');
                $out .= $this->whiteBox($ankety);
            }
        }

        $out .= '<div><a href="https://www.facebook.com/groups/44213111683/" target="_blank">';
        $out .= '<img alt="Facebook TK Olymp Olomouc" src="/style/fb-logo.png" />';
        $out .= '</a></div>';

        return $out;
    }

    public function render()
    {
        ;
    }
}
