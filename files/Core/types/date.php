<?php
class Date
{
    private $_valid;
    private $_sqlFormat;
    private $_year;
    private $_month;
    private $_day;
    private $_separators;

    const FORMAT_SQL = 'yyyy-mm-dd';
    const FORMAT_SIMPLE = 'dd.mm.yyyy';
    const FORMAT_SIMPLE_SPACED = 'dd. mm. yyyy';
    const FORMAT_SIMPLIFIED = 'd. m. yyyy';
    const FORMAT_SLASHED = 'dd/mm/yyyy';

    public function __construct($s = null) {
        $this->_separators = array('-', '.' , '/');
        if (is_string($s))
            $this->setDate($s);
    }
    public function __toString() {
        return $this->getDate();
    }
    public function separator($s = null, $reset = false) {
        if ($s === null)
            return $this->_separators;
        if ($reset)
            $this->_separators = array();
        $this->_separators[] = $s;
    }
    public function setDate($s) {
        foreach ($this->_separators as $sep) {
            if (strpos($s, $sep) === false)
                continue;
            $pieces = explode($sep, $s);
            if (count($pieces) != 3)
                unset($pieces);
        }
        if (!isset($pieces)) {
            if (!$this->_sqlFormat)
                $this->_valid = false;
            return false;
        }
        foreach ($pieces as &$piece) {
            $piece = trim($piece, '/-. ');
            if (strlen($piece) == 1)
                $piece = '0' . $piece;
        }
        if (strlen($pieces[2]) == 4) {
            $pieces = array_reverse($pieces);
        } elseif (strlen($pieces[0]) != 4) {
            if (!$this->_sqlFormat)
                $this->_valid = false;
            return false;
        }
        $this->_year = $pieces[0];
        $this->_month = $pieces[1];
        $this->_day = $pieces[2];
        $this->_sqlFormat = implode('-', $pieces);
        $this->_valid = true;
        return true;
    }
    public function getDate($format = Date::FORMAT_SQL) {
        if (!$this->_valid)
            return '';
        switch($format) {
            case Date::FORMAT_SQL:
                return $this->_sqlFormat;
            case Date::FORMAT_SIMPLE:
                return $this->_day . '.' . $this->_month . '.' . $this->_year;
                break;
            case Date::FORMAT_SIMPLE_SPACED:
                return $this->_day . '. ' . $this->_month . '. ' . $this->_year;
                break;
            case Date::FORMAT_SIMPLIFIED:
                return ((int) $this->_day) . '. ' .
                    ((int) $this->_month) . '. ' . $this->_year;
                break;
            case Date::FORMAT_SLASHED:
                return $this->_day . '/' . $this->_month . '/' . $this->_year;
                break;
            default:
                return '';
        }
    }
    public function getDay() {
        return $this->_day;
    }
    public function getMonth() {
        return $this->_month;
    }
    public function getYear() {
        return $this->_year;
    }
    public function isValid() {
        return $this->_valid;
    }
}