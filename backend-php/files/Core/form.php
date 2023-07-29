<?php
class Form
{
    private $valid = true;
    private $messages = [];

    public function isValid()
    {
        return $this->valid;
    }

    public function getMessages()
    {
        return $this->messages;
    }

    public function checkDate($i, $message)
    {
        if (preg_match('/^((?:19|20)\d\d)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/i', $i)) {
            list($year, $month, $day) = explode('-', $i);
            $year = intval($year);
            if (!($day == 31 && ($month == 4 || $month == 6 || $month == 9 || $month == 11)
                || $day >= 30 && $month == 2
                || $month == 2 && $day == 29 && !($year % 4 == 0 && ($year % 100 != 0 || $year % 400 == 0))
                )
            ) {
                return;
            }
        }
        $this->valid = false;
        $this->messages[] = $message;
    }

    public function checkTime($i, $message)
    {
        return $this->checkRegexp($i, '/^[012]?\d[:\.]\d\d$/', $message);
    }

    public function checkEmail($i, $message)
    {
        return $this->checkRegexp($i, '/^\b[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}\b$/i', $message);
    }

    public function checkPhone($i, $message)
    {
        return $this->checkRegexp($i, '/^((\+|00)\d{3})?( ?\d{3}){3}$/', $message);
    }

    public function checkLogin($i, $message)
    {
        $this->checkRegexp($i, '/^[A-Z0-9_]{3,20}$/i', $message);
        $row = \Database::querySingle("SELECT u_id FROM users WHERE u_login='?'", strtolower($i));
        $this->checkBool(!$row, 'Toto přihlašovací jméno je obsazené.');
    }

    public function checkPassword($i, $message)
    {
        return $this->checkRegexp($i, '/^[A-Z0-9_]{6,}$/i', $message);
    }

    public function checkRegexp($i, $regexp, $message)
    {
        if (!preg_match($regexp, $i)) {
            $this->valid = false;
            $this->messages[] = $message;
        }
    }

    public function checkInArray($i, $array, $message)
    {
        if (!in_array($i, $array)) {
            $this->valid = false;
            $this->messages[] = $message;
        }
    }

    public function checkNotEmpty($i, $message)
    {
        if (!$i && $i !== 0) {
            $this->valid = false;
            $this->messages[] = $message;
        }
    }

    public function checkNumeric($i, $message)
    {
        if (!is_numeric($i)) {
            $this->valid = false;
            $this->messages[] = $message;
        }
    }

    public function checkBool($i, $message)
    {
        if (!(bool) $i) {
            $this->valid = false;
            $this->messages[] = $message;
        }
    }
}
