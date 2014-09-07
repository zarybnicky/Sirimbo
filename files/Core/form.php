<?php
class Form
{
    private $_valid;
    private $_messages;
    private $_fields;

    const REGEXP_DATE = '/^((?:19|20)\d\d)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/i';
    const REGEXP_EMAIL = '/^\b[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b$/i';
    const REGEXP_PHONE = '/^((\+|00)\d{3})?( ?\d{3}){3}$/';
    const REGEXP_LOGIN = '/^[A-Z0-9_]{3,20}$/i';
    const REGEXP_PASSWORD = '/^[A-Z0-9_]{6,32}$/i';

    function __construct() {
        $this->_valid = true;
        $this->_messages = array();
        $this->_fields = array();
    }
    function isValid() {
        return $this->_valid;
    }
    function getMessages() {
        return $this->_messages;
    }
    function getFields() {
        return $this->_fields;
    }
    function checkDate($i, $message, $name = '') {
        if (preg_match(Form::REGEXP_DATE, $i)) {
            list($year, $month, $day) = explode('-', $i);
            if (
                !($day == 31 && ($month == 4 || $month == 6 || $month == 9 || $month == 11)
                || $day >= 30 && $month == 2
                || $month == 2 && $day == 29 && !($year % 4 == 0 && ($year % 100 != 0 || $year % 400 == 0))
                )
            ) {
                return true;
            }
        }
        $this->_error($message, $name);
        return false;
    }
    function checkEmail($i, $message, $name = '') {
        return $this->checkRegexp($i, Form::REGEXP_EMAIL, $message, $name);
    }
    function checkPhone($i, $message, $name = '') {
        return $this->checkRegexp($i, Form::REGEXP_PHONE, $message, $name);
    }
    function checkLogin($i, $message, $name = '') {
        return $this->checkRegexp($i, Form::REGEXP_LOGIN, $message, $name);
    }
    function checkPassword($i, $message, $name = '') {
        return $this->checkRegexp($i, Form::REGEXP_PASSWORD, $message, $name);
    }
    function checkRegexp($i, $regexp, $message, $name = '') {
        if (preg_match($regexp, $i))
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkLength($i, $min, $max, $message, $name = '') {
        $len = strlen($i);
        if ($len >= $min && $len <= $max)
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkInArray($i, $array, $message, $name = '') {
        if (in_array($i, $array))
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkArrayKey($i, $array, $message, $name = '') {
        if (array_key_exists($i, $array))
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkNotEmpty($i, $message, $name = '') {
        if ($i || $i === 0)
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkNumeric($i, $message, $name = '') {
        if (is_numeric($i))
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkNumberBetween($i, $min, $max, $message, $name) {
        if (is_numeric($i) && $i >= $min && $i <= $max)
            return true;

        $this->_error($message, $name);
        return false;
    }
    function checkBool($i, $message, $name) {
        if ((bool) $i)
            return true;

        $this->_error($message, $name);
        return false;
    }
    private function _error($message, $name = '') {
        $this->_valid = false;
        $this->_messages[] = $message;
        if ($name !== '')
            $this->_fields[] = $name;
    }
}
