<?php
class SelectHelper
{
    private $_name;
    private $_value;
    private $_options;

    public function select($n = null) {
        $this->_name = $n ?: '';
        $this->_value = null;
        $this->_options = array();

        return $this;
    }

    public function name($name) {
        $this->_name = $name;
        return $this;
    }

    public function set($value) {
        $this->_value = $value;
        return $this;
    }

    public function option($value, $name = null, $overwrite = false) {
        if ($overwrite)
            $this->_options = array();

        if ($name !== null)
            $this->_options[$value] = $name;
        elseif ($name === null)
            $this->_options[$value] = $value;
        return $this;
    }

    public function options($options = array(), $overwrite = false, $literal = false) {
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

    public function __toString() {
        return $this->render();
    }

    public function render() {
        $out = '<select name="' . $this->_name . '">' . "\n";
        if (!empty($this->_options)) {
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
