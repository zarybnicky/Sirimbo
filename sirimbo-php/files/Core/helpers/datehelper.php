<?php
class DateHelper
{
    protected $date;
    protected $dateTo;
    protected $name;
    protected $post;
    protected $useRange;
    protected $cls;

    public function date($name, $value = null)
    {
        $this->date = null;
        $this->name = $name;
        $this->post = true;
        $this->useRange = false;
        $this->cls = 'form-control';

        if ($value) {
            list($from, $to) = $this->createDate($value);
            $this->date = $from;
            $this->dateTo = $to;
        }

        return $this;
    }

    protected function createDate($date)
    {
        if (is_a($date, 'Date')) {
            return [$date, null];
        }
        if (!is_string($date)) {
            return [null, null];
        }
        if (strpos($date, ' - ')) {
            $pieces = explode(' - ', $date);
            $from = new Date($pieces[0]);
            $to = new Date($pieces[1]);
            return [$from->isValid() ? $from : null, $to->isValid() ? $to : null];
        } else {
            $date = new Date($date);
            return [$date->isValid() ? $date : null, null];
        }
    }

    public function name($name)
    {
        $this->name = $name;
        return $this;
    }

    public function set($d)
    {
        $this->date = $this->createDate($d);
        return $this;
    }

    public function setFromDate($d)
    {
        $this->date = $this->createDate($d);
        return $this;
    }

    public function setToDate($d)
    {
        $this->dateTo = $this->createDate($d);
        return $this;
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

    public function getPostRange($request)
    {
        if (!$request->post($this->name)) {
            return ['from' => new Date(), 'to' => new Date()];
        }
        if (strpos($request->post($this->name), ' - ')) {
            $pieces = explode(' - ', $request->post($this->name));
            $from = new Date($pieces[0]);
            $to = new Date($pieces[1]);
        }
        if (!isset($from) || !isset($to) || (!$from->isValid() && !$to->isValid())) {
            return ['from' => $this->getPost($request), 'to' => new Date()];
        }
        return ['from' => $from, 'to' => $to];
    }

    public function getPost($request)
    {
        return new Date($request->post($this->name));
    }

    public function __toString()
    {
        return $this->render();
    }

    public function render()
    {
        $val = null;

        if ($this->useRange) {
            if ($this->date && $this->dateTo) {
                $from = $this->date;
                $to = $this->dateTo;
            } else {
                $from = new Date();
                $to = new Date();
            }
            if ($from->isValid() || $from->isValid()) {
                $val = $from->getDate(Date::FORMAT_SIMPLIFIED)
                    . ' - '
                    . $to->getDate(Date::FORMAT_SIMPLIFIED);
            }
        }
        if (!$val) {
            $val = $this->date ? $this->date->getDate(Date::FORMAT_SIMPLIFIED) : '';
        }
        return (string) new Tag(
            'input',
            ['type' => 'text', 'name' => $this->name, 'value' => $val, 'class' => $this->cls]
        );
    }
}