<?php
class Sidebar {
    protected $data;
    protected $toplevel;

    public function __construct($data) {
        $this->data = $data;
    }

    public function __toString() {
        $out = '<div class="container full menu-side"><ul>';

        foreach ($this->data as $item) {
            if (!empty($item[2])) {
                $out .= '<li class="more">';
            } else {
                $out .= '<li>';
            }


            if (strripos(Request::getLiteralURI(), trim($item[1], '/')) === 0) {
                $out .= '<a class="emph no-a">';
            } elseif ($item[1]) {
                $out .= '<a href="' . $item[1] . '">';
            } else {
                $out .= '<a>';
            }
            $out .= $item[0];
            $out .= '</a>';

            $out .= '</li>';
        }

        return $out . '</ul></div>';
    }
}
