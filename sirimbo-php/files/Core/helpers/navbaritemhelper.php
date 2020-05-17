<?php
class NavbarItemHelper
{
    protected $item;

    public function navbarItem($item)
    {
        $this->item = $item;
        return $this;
    }

    public function render()
    {
        $item = $this->item;

        if (!is_array($item)) {
            return "<div class='$item'></div>";
        }
        if (isset($item[3]) && $item[3] && !Permissions::check($item[3][0], $item[3][1])) {
            return '';
        }
        $active = $item[1] === ('/' . Database::$request->getURI())
            || (strlen($item[1]) > 1 && strpos('/' . Database::$request->getURI(), $item[1]) === 0);
        $active = $active ? ' active' : '';

        if (!isset($item[2]) || empty($item[2])) {
            return '<li class="nav-item' . $active . '"><a class="nav-link" href="'
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
        return '<li class="nav-item' . $active . ' dropdown">' . $x . '</li>';
    }

    public function __toString()
    {
        return $this->render();
    }
}
