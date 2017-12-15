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
                return ['from' => $this->getPost($request, true), 'to' => new Date()];
            }

            return ['from' => $from, 'to' => $to];
        } elseif ($request->get($this->name)) {
            if (strpos($request->get($this->name), ' - ')) {
                $pieces = explode(' - ', $request->get($this->name));
                $from = new Date($pieces[0]);
                $to = new Date($pieces[1]);
            }
            if (!isset($from) || !isset($to) || (!$from->isValid() && !$to->isValid())) {
                return ['from' => $this->getPost($request, true), 'to' => new Date()];
            }

            return ['from' => $from, 'to' => $to];
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
                return ['from' => $this->getPost($request, true), 'to' => new Date()];
            }

            return ['from' => $from, 'to' => $to];
        } else {
            return ['from' => new Date(), 'to' => new Date()];
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
        //TODO:ranged select...
        if ($this->view == 'select') {
            $s = Helper::instance()->select();

            $s->name($this->name . '-day')
              ->value($this->date ? $this->date->getDay() : null)
              ->options([], true)
              ->option('00', 'Den');
            for ($i = 1; $i < 32; $i++) {
                $s->option((($i < 10) ? ('0' . $i) : $i), $i);
            }
            $out .= $s;

            $out .= $s->name($this->name . '-month')
                ->value($this->date ? $this->date->getMonth() : null)
                ->options([
                    '00' => 'Měsíc',
                    '01' => 'Leden',
                    '02' => 'Únor',
                    '03' => 'Březen',
                    '04' => 'Duben',
                    '05' => 'Květen',
                    '06' => 'Červen',
                    '07' => 'Červenec',
                    '08' => 'Srpen',
                    '09' => 'Září',
                    '10' => 'Říjen',
                    '11' => 'Listopad',
                    '12' => 'Prosinec'
                ], true);

            $s->name($this->name . '-year')
                ->value($this->date ? $this->date->getYear() : null)
                ->options([], true)
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
                    $selected = $from->getDate(Date::FORMAT_SIMPLIFIED)
                              . ' - '
                              . $to->getDate(Date::FORMAT_SIMPLIFIED);
                    $done = true;
                }
            }
            if (!$done) {
                $selected = $this->date ? $this->date->getDate(Date::FORMAT_SIMPLIFIED) : '';
            }
            $out .= new Tag(
                'input',
                ['type' => 'text', 'name' => $this->name, 'value' => $selected]
            );
        }
        return $out;
    }
}
