<?php
namespace TKOlomouc\Type;

class DateFormat
{
    private $valid;
    private $sqlFormat;
    private $year;
    private $month;
    private $day;
    private $separators;

    const FORMAT_SQL = 'yyyy-mm-dd';
    const FORMAT_SIMPLE = 'dd.mm.yyyy';
    const FORMAT_SIMPLE_SPACED = 'dd. mm. yyyy';
    const FORMAT_SIMPLIFIED = 'd. m. yyyy';
    const FORMAT_SLASHED = 'dd/mm/yyyy';

    public function __construct($s = null)
    {
        $this->separators = array('-', '.' , '/');
        if (is_string($s)) {
            $this->setDate($s);
        }
    }

    public function __toString()
    {
        return $this->getDate();
    }

    public function separator($s = null, $reset = false)
    {
        if ($s === null) {
            return $this->separators;
        }
        if ($reset) {
            $this->separators = array();
        }
        $this->separators[] = $s;
    }

    public function setDate($s)
    {
        foreach ($this->separators as $sep) {
            if (strpos($s, $sep) === false) {
                continue;
            }
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

    public function getDate($format = self::FORMAT_SQL)
    {
        if (!$this->valid) {
            return '';
        }
        switch($format) {
            case self::FORMAT_SQL:
                return $this->sqlFormat;
            case self::FORMAT_SIMPLE:
                return $this->day . '.' .     $this->month . '.' . $this->year;
            case self::FORMAT_SIMPLE_SPACED:
                return $this->day . '. ' .     $this->month . '. ' . $this->year;
            case self::FORMAT_SIMPLIFIED:
                return ((int) $this->day) . '. '
                    . ((int) $this->month) . '. ' . $this->year;
            case self::FORMAT_SLASHED:
                return $this->day . '/' .     $this->month . '/' . $this->year;
            default:
                return '';
        }
    }

    public function getDay()
    {
        return $this->day;
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
