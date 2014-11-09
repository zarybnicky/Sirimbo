<?php
class Sidebar
{
    public static $hasSidebarDiv = false;

    public function __construct() {
        if (!Sidebar::$hasSidebarDiv)
            echo '<style type="text/css">#content{margin-left:110px !important;}</style>';
        Sidebar::$hasSidebarDiv = true;
    }

    public function menuHeader() {
        return $this->blackBox('<span class="logo"></span>Menu');
    }
    public function menuItem($text, $link, $module = '', $permission = P_NONE) {
        if ($module != '' && !Permissions::check($module, $permission))
            return;

        $active = stripos(Request::getURI(), $link) === 0;
        $r = '<span class="arrow">.</span>' . $text;
        if ($active)
            return $this->blackBox($r . '<span class="point">.</span>', 'sidebar current', $link);
        else
            return $this->whiteBox($r, 'sidebar', $link);
    }
    public function blackBox($text, $class = '', $link = '') {
        $text = '<div class="dark-in">' . $text . '</div>';
        if ($link != '') {
            $text = '<a href="' . $link . '">' . $text . '</a>';
        }
        return '<div class="dark-out' .
            ($class ? ' ' . $class : '') . '">' . $text . '</div>';
    }
    public function whiteBox($text, $class = '', $link = '') {
        $text = '<div class="light-in">' . $text . '</div>';
        if ($link != '') {
            $text = '<a href="' . $link . '">' . $text . '</a>';
        }
        return '<div class="light-out' .
            ($class ? ' ' . $class : '') . '">' . $text . '</div>';
    }
}
