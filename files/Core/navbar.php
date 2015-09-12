<?php
class Navbar {
    protected $data;
    protected $toplevel;

    public function __construct($data, $toplevel = false) {
        $this->data = $data;
        $this->toplevel = $toplevel;
    }

    public function __toString() {
        if ($this->toplevel) {
            $out = '<div id="menu">';
            $out .= '<div id="menu-logo">';
            $out .= '  <img alt="" src="/style/logo-small.png" />';
            $out .= '</div>';
            $out .= '<ul class="menu-inner">';
        } else {
            $out = '<div class="submenu">';
            $out .= '<ul class="menu-inner">';
        }

        foreach ($this->data as $item) {
            if (
                isset($item[3])
                && $item[3]
                && !Permissions::check($item[3][0], $item[3][1])
            ) {
                continue;
            }

            if (isset($item[4]) && $item[4] && !empty($item[2])) {
                $out .= '<li class="menu-more menu-right">';
            } elseif (isset($item[4]) && $item[4]) {
                $out .= '<li class="menu-right">';
            } elseif (!empty($item[2])) {
                $out .= '<li class="menu-more">';
            } else {
                $out .= '<li>';
            }

            if ($item[1]) {
                $out .= '<a href="' . $item[1] . '">';
            } else {
                $out .= '<a>';
            }
            $out .= $item[0];
            $out .= '</a>';

            if (!empty($item[2])) {
                $out .= '<ul>';
                foreach ($item[2] as $subitem) {
                    if (
                        isset($subitem[3])
                        && $subitem[3]
                        && !Permissions::check($subitem[3][0], $subitem[3][1])
                    ) {
                        continue;
                    }

                    $out .= '<li>';
                    if ($subitem[1]) {
                        $out .= '<a href="' . $subitem[1] . '">';
                    } else {
                        $out .= '<a>';
                    }
                    $out .= $subitem[0] . '</a></li>';
                }
                $out .= '</ul>';
            }

            $out .= '</li>';
        }

        return $out . '</ul></div>';
    }
}
