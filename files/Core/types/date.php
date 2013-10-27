<?php
class Date {
    private $valid;
    private $sql_format;
    private $year;
    private $month;
    private $day;
    private $separators;
    
    const FORMAT_SQL = 'yyyy-mm-dd';
    const FORMAT_SIMPLE = 'dd.mm.yyyy';
    const FORMAT_SIMPLE_SPACED = 'dd. mm. yyyy';
    const FORMAT_SIMPLIFIED = 'd. m. yyyy';
    const FORMAT_SLASHED = 'dd/mm/yyyy';
    
    function __construct($s = null) {
        $this->separators = array('-', '.' , '/');
        if(is_string($s))
            $this->setDate($s);
    }
    function __toString() {
        return $this->getDate();
    }
    function separator($s = null, $reset = false) {
        if($s === null)
            return $this->separators;
        if($reset)
            $this->separators = array();
        $this->separators[] = $s;
    }
    function setDate($s) {
        foreach($this->separators as $sep) {
            if(strpos($s, $sep) === false)
                continue;
            $pieces = explode($sep, $s);
            if(count($pieces) != 3)
                unset($pieces);
        }
        if(!isset($pieces)) {
            if(!$this->sql_format)
                $this->valid = false;
            return false;
        }
        foreach($pieces as &$piece) {
            $piece = trim($piece, '/-. ');
            if(strlen($piece) == 1)
                $piece = '0' . $piece;
        }
        if(strlen($pieces[2]) == 4) {
            $pieces = array_reverse($pieces);
        } elseif(strlen($pieces[0]) != 4) {
            if(!$this->sql_format)
                $this->valid = false;
            return false;
        }
        $this->year = $pieces[0];
        $this->month = $pieces[1];
        $this->day = $pieces[2];
        $this->sql_format = implode('-', $pieces);
        $this->valid = true;
        return true;
    }
    function getDate($format = Date::FORMAT_SQL) {
        if(!$this->valid)
            return '';
        switch($format) {
            case Date::FORMAT_SQL:
                return $this->sql_format;
            case Date::FORMAT_SIMPLE:
                return $this->day . '.' .     $this->month . '.' . $this->year;
                break;
            case Date::FORMAT_SIMPLE_SPACED:
                return $this->day . '. ' .     $this->month . '. ' . $this->year;
                break;
            case Date::FORMAT_SIMPLIFIED:
                return ((int) $this->day) . '. ' .
                    ((int) $this->month) . '. ' . $this->year;
                break;
            case Date::FORMAT_SLASHED:
                return $this->day . '/' .     $this->month . '/' . $this->year;
                break;
            default:
                return '';
        }
    }
    function getDay() {
        return $this->day;
    }
    function getMonth() {
        return $this->month;
    }
    function getYear() {
        return $this->year;
    }
    function isValid() {
        return $this->valid;
    }
}