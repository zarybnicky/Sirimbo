<?php
class PagingAdapterArray implements PagingAdapterInterface {
    private $array;
    
    function __construct($a = null) {
        $this->setArray($a);
    }
    function setArray($a) {
        if(!is_array($a))
            return false;
        $this->array = $a;
    }
    
    function page($offset, $length, $options = null) {
        return array_slice($this->array, $offset, $lenght);
    }
    function count($options = null) {
        return count($this->array);
    }
}