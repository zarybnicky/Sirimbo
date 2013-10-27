<?php
class SelectHelper {
    private $name;
    private $value;
    private $options;
    private $get;
    
    function select($n = null) {
        $this->_defaultValues();
        
        if($n !== null)
            return $this->name($n);
        return $this;
    }
    function _defaultValues() {
        $this->name = '';
        $this->value = null;
        $this->options = array();
        $this->get = false;
    }
    
    function name($name = '') {
        if($name)
            $this->name = $name;
        return $this;
    }
    function value($value = null) {
        if($value)
            $this->value = $value;
        return $this;
    }
    function option($value, $name = null, $overwrite = false) {
        if($overwrite)
            $this->options = array();
        
        if($name !== null)
            $this->options[$value] = $name;
        elseif($name === null)
            $this->options[$value] = $value;
        return $this;
    }
    function options($options = array(), $overwrite = false, $literal = false) {
        if($overwrite === true)
            $this->options = array();
        
        if(!is_array($options) || empty($options))
            return $this;
        
        foreach($options as $value => $name) {
            if(!isset($name))
                continue;
            if($literal)
                $this->options[$name] = $name;
            else
                $this->options[$value] = $name;
        }
        return $this;
    }
    function get($get = true) {
        $this->get = (bool) $get;
        return $this;
    }
    function post($post = true) {
        $this->get = !(bool) $post;
        return $this;
    }
    
    function __toString() {
        return $this->render();
    }
    function render() {
        $out = '<select name="' . $this->name . '">' . "\n";
        if(!empty($this->options)) {
            $selected = $this->get ? get($this->name) : post($this->name);
            if($selected === null)
                $selected = $this->value;
            foreach($this->options as $value => $name) {
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