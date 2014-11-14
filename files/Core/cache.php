<?php
class Cache
{
    private $_content;
    private $_valid = false;
    private $_name = '';
    private $_time;

    public function __construct($name, $content = false) {
        $this->reset($name, $content);
    }

    public function reset($name, $new = false) {
        $this->_name = $name;
        $this->_content = $new;
        $this->_valid = true;
        $this->_time = time();
    }
    public function set($new) {
        $this->_content = $new;
        $this->_valid = true;
        $this->_time = time();
    }
    public function get() {
        if ($this->_valid == true)
            return $this->_content;
        else
            return false;
    }
    public function setName($name) {
        $this->_name = $name;
    }
    public function getName() {
        return $this->_name;
    }
    public function getTime() {
        return $this->_time;
    }
    public function invalidate() {
        $this->_valid = false;
    }
    public function isValid() {
        return $this->_valid;
    }
}
