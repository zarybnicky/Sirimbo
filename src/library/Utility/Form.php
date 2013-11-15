<?php
namespace TKOlomouc\Utility;

class Form
{
    private $valid    = true;
    private $messages = array();
    private $fields   = array();

    private final $regexpDate     = '/^((?:19|20)\d\d)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/i';
    private final $regexpEmail    = '/^\b[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b$/i';
    private final $regexpPhone    = '/^((\+|00)\d{3})?( ?\d{3}){3}$/';
    private final $regexpLogin    = '/^[A-Z0-9_]{3,20}$/i';
    private final $regexpPassword = '/^[A-Z0-9_]{6,32}$/i';

    function isValid()
    {
        return $this->valid;
    }

    function getMessages()
    {
        return $this->messages;
    }

    function getFields()
    {
        return $this->fields;
    }

    function checkDate($i, $message, $name = '')
    {
        if (preg_match($this->regexpDate, $i)) {
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
        $this->error($message, $name);
        return false;
    }

    function checkEmail($i, $message, $name = '')
    {
        return $this->checkRegexp($i, $this->regexpEmail, $message, $name);
    }

    function checkPhone($i, $message, $name = '')
    {
        return $this->checkRegexp($i, $this->regexpPhone, $message, $name);
    }

    function checkLogin($i, $message, $name = '')
    {
        return $this->checkRegexp($i, $this->regexpLogin, $message, $name);
    }

    function checkPassword($i, $message, $name = '')
    {
        return $this->checkRegexp($i, $this->regexpPassword, $message, $name);
    }

    function checkRegexp($i, $regexp, $message, $name = '')
    {
        if (preg_match($regexp, $i)) {
            return true;
        }
        $this->error($message, $name);
        return false;
    }

    function checkLength($i, $min, $max, $message, $name = '')
    {
        $len = strlen($i);
        if ($len >= $min && $len <= $max) {
            return true;
        }
        $this->error($message, $name);
        return false;
    }

    function checkInArray($i, $array, $message, $name = '')
    {
        if (in_array($i, $array)) {
            return true;
        }
        $this->error($message, $name);
        return false;
    }

    function checkArrayKey($i, $array, $message, $name = '')
    {
        if (array_key_exists($i, $array)) {
            return true;
        }
        $this->error($message, $name);
        return false;
    }

    function checkNotEmpty($i, $message, $name = '')
    {
        if ($i || $i === 0) {
            return true;
        }
        $this->error($message, $name);
        return false;
    }

    function checkNumeric($i, $message, $name = '')
    {
        if (is_numeric($i)) {
            return true;
        }
        $this->error($message, $name);
        return false;
    }

    function checkNumberBetween($i, $min, $max, $message, $name)
    {
        if (is_numeric($i) && $i >= $min && $i <= $max) {
            return true;
        }
        $this->error($message, $name);
        return false;
    }

    function checkBool($i, $message, $name)
    {
        if ((bool) $i) {
            return true;
        }
        $this->error($message, $name);
        return false;
    }

    private function error($message, $name = null)
    {
        $this->valid = false;
        $this->messages[] = $message;
        if ($name !== null) {
            $this->fields[] = $name;
        }
    }
}
