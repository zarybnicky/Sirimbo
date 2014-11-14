<?php
class MenuHelper
{
    private $_left;
    private $_right;
    private $_float;
    private $_content;

    const FLOAT_NONE = '0';
    const FLOAT_LEFT = 1;
    const FLOAT_RIGHT = 2;

    public function menu() {
        $this->_defaultValues();
        return $this;
    }
    private function _defaultValues() {
        $this->_left = -1;
        $this->_right = 0;
        $this->_float = 0;
        $this->_content = array();
    }
    public function left($left) {
        if ($left >= -1)
            $this->_left = $left;
        return $this;
    }
    public function right($right) {
        if ($right >= -1)
            $this->_right = $right;
        return $this;
    }
    public function float($type) {
        if (is_int($type))
            $this->_float = $type;
        return $this;
    }
    public function content($name, $url, $button = false, $replace = false) {
        if (!$name || !$url)
            return $this;
        if ($replace)
            $this->_content = array($name => array($url, $button));
        else
            $this->_content[$name] = array($url, $button);

        return $this;
    }
    public function render() {
        $out = '<div class="sticky" style="width:150px;';
        if ($this->_right > -1) $out .= 'margin-left:' . $this->_right . 'px;';
        if ($this->_left > -1) $out .= 'margin-right:' . $this->_left . 'px;';
        switch($this->_float) {
            case MenuHelper::FLOAT_RIGHT: $out .= 'float:right;'; break;
            case MenuHelper::FLOAT_LEFT: $out .= 'float:left;'; break;
            case MenuHelper::FLOAT_NONE: $out .= 'float:none;'; break;
        }
        $out .= '">';
        ?>
        <script type="text/javascript">
        (function($) {
            $(function() {
                if (typeof $.fn.topFeatures == "undefined") {
                    $.getScript("/scripts/jquery.sticky.js", function() {
                        $(".sticky").sticky({topSpacing:15});
                    });
                } else {
                    $(".sticky").sticky({topSpacing:15});
                }
            })
        })(jQuery);
        </script>
        <?php
        $out .= '<div style="z-index:100;width:inherit;' .
            'border:1px solid #aaa;background:#ddd;margin-top:2px;padding:3px 1px;">';
        if (!empty($this->_content)) {
            $i = 1;
            foreach ($this->_content as $name => $data) {
                if ($data[1])
                    $out .= '<button style="padding:0" name="action" value="' . $data[0] .
                        '">' . $name . '</button>';
                else
                    $out .= '<a style="padding:0 3px" href="' . $data[0] . '">' . $name . '</a>';
                if ($i++ < count($this->_content))
                    $out .= '<br/>';
            }
        }
        $out .= '</div>';
        $out .= '</div>';
        return $out;
    }
    public function __toString() {
        return $this->render();
    }
}