<?php
class SelectHelper
{
    private $_name;
    private $_value;
    private $_options;
    private $_get;

    function select($n = null) {
        $this->_defaultValues();

        if ($n !== null)
            return $this->name($n);
        return $this;
    }
    private function _defaultValues() {
        $this->_name = '';
        $this->_value = null;
        $this->_options = array();
        $this->_get = false;
    }

    function name($name = '') {
        if ($name)
            $this->_name = $name;
        return $this;
    }
    function value($value = null) {
        if ($value)
            $this->_value = $value;
        return $this;
    }
    function option($value, $name = null, $overwrite = false) {
        if ($overwrite)
            $this->_options = array();

        if ($name !== null)
            $this->_options[$value] = $name;
        elseif ($name === null)
            $this->_options[$value] = $value;
        return $this;
    }
    function options($options = array(), $overwrite = false, $literal = false) {
        if ($overwrite === true)
            $this->_options = array();

        if (!is_array($options) || empty($options))
            return $this;

        foreach ($options as $value => $name) {
            if (!isset($name))
                continue;
            if ($literal)
                $this->_options[$name] = $name;
            else
                $this->_options[$value] = $name;
        }
        return $this;
    }
    function get($get = true) {
        $this->_get = (bool) $get;
        return $this;
    }
    function post($post = true) {
        $this->_get = !(bool) $post;
        return $this;
    }

    function __toString() {
        return $this->render();
    }
    function render() {
        $out = '<select name="' . $this->_name . '">' . "\n";
        if (!empty($this->_options)) {
            $selected = $this->_get ? get($this->_name) : post($this->_name);
            if ($selected === null)
                $selected = $this->_value;
            foreach ($this->_options as $value => $name) {
                $out .= '<option value="' . $value . '"' .
                    ($selected == $value ? ' selected="selected"' : '') .
                    '>' . $name . '</option>' . "\n";
            }
        }
        $out .= '</select>' . "\n";

        return $out;
    }
}
?>