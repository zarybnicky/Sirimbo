<?php
class Cache
{
    private $_content;
    private $_valid = false;
    private $_name = '';
    private $_time;

    function __construct($name, $content = false) {
        $this->reset($name, $content);
    }

    function reset($name, $new = false) {
        $this->_name = $name;
        $this->_content = $new;
        $this->_valid = true;
        $this->_time = time();
    }
    function set($new) {
        $this->_content = $new;
        $this->_valid = true;
        $this->_time = time();
    }
    function get() {
        if ($this->_valid == true)
            return $this->_content;
        else
            return false;
    }
    function setName($name) {
        $this->_name = $name;
    }
    function getName() {
        return $this->_name;
    }
    function getTime() {
        return $this->_time;
    }
    function invalidate() {
        $this->_valid = false;
    }
    function isValid() {
        return $this->_valid;
    }
}
?>