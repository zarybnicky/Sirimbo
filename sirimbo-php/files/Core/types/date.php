<?php
class Date
{
    protected $valid;
    protected $sqlFormat;
    protected $year;
    protected $month;
    protected $day;
    protected static $separators = ['-', '.' , '/'];

    public function __construct($s = null)
    {
        if (is_string($s)) {
            $this->setDate($s);
        }
    }

    public function __toString()
    {
        return $this->getDate();
    }

    public function setDate($s)
    {
        foreach (self::$separators as $sep) {
            if (strpos($s, $sep) === false) {
                continue;
            }
            /** @var array */
            $pieces = explode($sep, $s);
            if (count($pieces) != 3) {
                unset($pieces);
            }
        }
        if (!isset($pieces)) {
            if (!$this->sqlFormat) {
                $this->valid = false;
            }
            return false;
        }
        foreach ($pieces as &$piece) {
            $piece = trim($piece, '/-. ');
            if (strlen($piece) == 1) {
                $piece = '0' . $piece;
            }
        }
        if (strlen($pieces[2]) == 4) {
            $pieces = array_reverse($pieces);
        } elseif (strlen($pieces[0]) != 4) {
            if (!$this->sqlFormat) {
                $this->valid = false;
            }
            return false;
        }
        $this->year = $pieces[0];
        $this->month = $pieces[1];
        $this->day = $pieces[2];
        $this->sqlFormat = implode('-', $pieces);
        $this->valid = true;
        return true;
    }

    public function getDate()
    {
        if (!$this->valid) {
            return '';
        }
        return $this->sqlFormat;
    }

    public function getHumanDate()
    {
        if (!$this->valid) {
            return '';
        }
        return ((int) $this->day) . '. ' . ((int) $this->month) . '. ' . $this->year;
    }

    public function getMonth()
    {
        return $this->month;
    }

    public function getYear()
    {
        return $this->year;
    }

    public function isValid()
    {
        return $this->valid;
    }
}
