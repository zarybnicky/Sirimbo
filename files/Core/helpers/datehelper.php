<?php
class DateHelper
{
    protected $date;
    protected $dateTo;
    protected $view;
    protected $name;
    protected $post;
    protected $fromYear;
    protected $toYear;
    protected $useRange;

    public function date($name, $value = null)
    {
        $this->view = 'text';
        $this->date = null;
        $this->name = $name;
        $this->post = true;
        $this->fromYear = ((int) date('Y')) - 75;
        $this->toYear = ((int) date('Y')) + 5;
        $this->useRange = false;

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
            return array($date, null);
        }
        if (!is_string($date)) {
            return array(null, null);
        }
        if (strpos($date, ' - ')) {
            $pieces = explode(' - ', $date);
            $from = new Date($pieces[0]);
            $to = new Date($pieces[1]);
            return array(
                $from->isValid() ? $from : null,
                $to->isValid() ? $to : null
            );
        } else {
            $date = new Date($date);
            return array($date->isValid() ? $date : null, null);
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
        $this->date = $this->setDate($d);
        return $this;
    }

    public function setToDate($d)
    {
        $this->dateTo = $this->setDate($d);
        return $this;
    }

    public function selectBox()
    {
        $this->view = 'select';
        return $this;
    }

    public function textBox()
    {
        $this->view = 'text';
        return $this;
    }

    public function fromYear($y)
    {
        $this->fromYear = $y;
        return $this;
    }

    public function toYear($y)
    {
        if ($y > $this->fromYear) {
            $this->toYear = $y;
        } else {
            $this->toYear = $this->fromYear;
        }
        return $this;
    }

    public function range($b = true)
    {
        $this->useRange = (bool) $b;
        return $this;
    }

    public function getPostRange($request)
    {
        if ($request->post($this->name)) {
            if (strpos($request->post($this->name), ' - ')) {
                $pieces = explode(' - ', $request->post($this->name));
                $from = new Date($pieces[0]);
                $to = new Date($pieces[1]);
            }
            if (!isset($from) || !isset($to) || (!$from->isValid() && !$to->isValid())) {
                return array('from' => $this->getPost($request, true), 'to' => new Date());
            }

            return array('from' => $from, 'to' => $to);
        } elseif ($request->get($this->name)) {
            if (strpos($request->get($this->name), ' - ')) {
                $pieces = explode(' - ', $request->get($this->name));
                $from = new Date($pieces[0]);
                $to = new Date($pieces[1]);
            }
            if (!isset($from) || !isset($to) || (!$from->isValid() && !$to->isValid())) {
                return array('from' => $this->getPost($request, true), 'to' => new Date());
            }

            return array('from' => $from, 'to' => $to);
        } elseif (
            $request->post($this->name . '-from-year')
            && $request->post($this->name . '-from-month')
            && $request->post($this->name . '-from-day')
            && $request->post($this->name . '-to-year')
            && $request->post($this->name . '-to-month')
            && $request->post($this->name . '-to-day')
        ) {
            $from = new Date(
                $request->post($this->name . '-from-year') . '-'
                . $request->post($this->name . '-from-month') . '-'
                . $request->post($this->name . '-from-day')
            );
            $to = new Date(
                $request->post($this->name . '-to-year') . '-'
                . $request->post($this->name . '-to-month') . '-'
                . $request->post($this->name . '-to-day')
            );

            if (!$from->isValid() && !$to->isValid()) {
                return array('from' => $this->getPost($request, true), 'to' => new Date());
            }

            return array('from' => $from, 'to' => $to);
        } else {
            return array('from' => new Date(), 'to' => new Date());
        }
    }

    public function getPost($request, $name = null)
    {
        if ($name === null) {
            $name = $this->name;
        }
        if ($request->post($name)) {
            return new Date($request->post($name));
        } elseif ($request->post($name . '-year') &&
                  $request->post($name . '-month') &&
                  $request->post($name . '-day')
        ) {
            return new Date(
                $request->post($name . '-year') . '-'
                . $request->post($name . '-month') . '-'
                . $request->post($name . '-day')
            );
        } else {
            return new Date();
        }
    }

    public function __toString()
    {
        return $this->render();
    }

    public function render()
    {
        $out = '';
        //TODO:ranged select... ftf
        if ($this->view == 'select') {
            $s = Helper::instance()->select();

            $s->name($this->name . '-day')
              ->value($this->date ? $this->date->getDay() : null)
              ->options(array(), true)
              ->option('00', 'Den');
            for ($i = 1; $i < 32; $i++) {
                $s->option((($i < 10) ? ('0' . $i) : $i), $i);
            }
            $out .= $s;

            $out .= $s->name($this->name . '-month')
                ->value($this->date ? $this->date->getMonth() : null)
                ->options(array(), true)
                ->option('00', 'Měsíc')
                ->option('01', 'Leden')
                ->option('02', 'Únor')
                ->option('03', 'Březen')
                ->option('04', 'Duben')
                ->option('05', 'Květen')
                ->option('06', 'Červen')
                ->option('07', 'Červenec')
                ->option('08', 'Srpen')
                ->option('09', 'Září')
                ->option('10', 'Říjen')
                ->option('11', 'Listopad')
                ->option('12', 'Prosinec');

            $s->name($this->name . '-year')
                ->value($this->date ? $this->date->getYear() : null)
                ->options(array(), true)
                ->option('0000', 'Rok');
            for ($i = $this->fromYear; $i < $this->toYear; $i++) {
                $s->option($i, $i);
            }
            $out .= $s;
        } elseif ($this->view == 'text') {
            $done = false;

            if ($this->useRange) {
                if ($this->date && $this->dateTo) {
                    $from = $this->date;
                    $to = $this->dateTo;
                } else {
                    $from = new Date();
                    $to = new Date();
                }
                if ($from->isValid() || $from->isValid()) {
                    $selected = $from->getDate(Date::FORMAT_SIMPLIFIED) . ' - ' . $to->getDate(Date::FORMAT_SIMPLIFIED);
                    $done = true;
                }
            }
            if (!$done) {
                $selected = $this->date ? $this->date->getDate(Date::FORMAT_SIMPLIFIED) : '';
            }
            $out .= '<input type="text" name="' . $this->name . '" value="' . $selected . '" />' . "\n";
        }
        return $out;
    }
}
