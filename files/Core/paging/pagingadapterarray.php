<?php
class PagingAdapterArray implements PagingAdapterInterface
{
    private $_array;

    public function __construct($a = null) {
        $this->setArray($a);
    }
    public function setArray($a) {
        if (!is_array($a))
            return false;
        $this->_array = $a;
    }

    public function page($offset, $length, $options = null) {
        return array_slice($this->_array, $offset, $length);
    }
    public function count($options = null) {
        return count($this->_array);
    }
}
