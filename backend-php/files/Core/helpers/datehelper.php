<?php
class DateHelper
{
    protected $date;
    protected $dateTo;
    protected $name;
    protected $useRange;
    protected $cls;

    public function __construct($name, $date = null)
    {
        $this->date = null;
        $this->name = $name;
        $this->useRange = false;
        $this->cls = 'form-control';
        if (!$date) {
            return;
        }
        if (is_a($date, 'Date')) {
            $this->date = $date;
            return;
        }
        if (!is_string($date)) {
            return;
        }
        if (strpos($date, ' - ')) {
            $pieces = explode(' - ', $date);
            $this->date = new \Date(trim($pieces[0]));
            $this->dateTo = new \Date(trim($pieces[1]));
        } else {
            $this->date = new \Date($date);
        }
    }

    public function getFromDate()
    {
        return $this->date;
    }

    public function getToDate()
    {
        return $this->dateTo;
    }

    public function range($b = true)
    {
        $this->useRange = (bool) $b;
        return $this;
    }

    public function cls($cls)
    {
        $this->cls = $cls;
        return $this;
    }

    public static function getPostRange($name)
    {
        if (empty($_POST[$name])) {
            return ['from' => new \Date(), 'to' => new \Date()];
        }
        if (strpos($_POST[$name], '-') && strpos($_POST[$name], '-') === strrpos($_POST[$name], '-')) {
            $pieces = explode('-', $_POST[$name]);
            $from = new \Date(trim($pieces[0]));
            $to = new \Date(trim($pieces[1]));
        }
        if (!isset($from) || !isset($to) || (!$from->isValid() && !$to->isValid())) {
            return ['from' => new \Date($_POST[$name] ?? null), 'to' => new \Date()];
        }
        return ['from' => $from, 'to' => $to];
    }

    public function __toString(): string
    {
        if ($this->useRange) {
            if ($this->date && $this->dateTo) {
                $from = $this->date;
                $to = $this->dateTo;
            } else {
                $from = new \Date();
                $to = new \Date();
            }
            $val = '';
            if ($from->isValid() || $to->isValid()) {
                $val = $from->getHumanDate() . ' - ' . $to->getHumanDate();
            }
            return "<input type=\"text\" name=\"{$this->name}\" value=\"$val\" class=\"{$this->cls}\">";
        } else {
            $val = $this->date ? $this->date->getDate() : '';
            return "<input type=\"date\" name=\"{$this->name}\" value=\"$val\" class=\"{$this->cls}\">";
        }
    }
}