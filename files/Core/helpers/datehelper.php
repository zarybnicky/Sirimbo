<?php
/*
 * Example:
echo '<form action="" method="post">';
echo $this->date('2012-12-21')->name('test1')->selectBox(), '<br/>';
echo $this->date('2000-01-01')->name('test2')->textBox(), '<br/>';
echo $this->date()->name('test1')->getPost(), '<br/>';
echo $this->date()->name('test2')->getPost(), '<br/>';
echo '<input type="submit" value="Send" />';
echo '</form>';
*/
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

    public function date($d = null)
    {
        $this->view = 'text';
        $this->date = null;
        $this->name = null;
        $this->post = true;
        $this->fromYear = ((int) date('Y')) - 75;
        $this->toYear = ((int) date('Y')) + 5;
        $this->useRange = false;

        if ($d) {
            $date = $this->_setDate($d);
            if ($date) {
                $this->date = $date;
            } else {
                $this->name($d);
            }
        }

        return $this;
    }

    protected function _setDate($date)
    {
        if (!is_a($date, 'Date') && is_string($date)) {
            $date = new Date($date);
        }
        if (!is_a($date, 'Date') || !$date->isValid()) {
            return null;
        } else {
            return $date;
        }
    }

    public function setDate($d)
    {
        $this->date = $this->_setDate($d);
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

    public function name($name)
    {
        $this->name = $name;
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

    public function getPostRange()
    {
        if (!$this->useRange) {
            return array('from' => $this->getPost(), 'to' => new Date());
        }

        if (post($this->name)) {
            if (strpos(post($this->name), ' - ')) {
                $pieces = explode(' - ', post($this->name));
                $from = new Date($pieces[0]);
                $to = new Date($pieces[1]);
            }
            if (!isset($from) || !isset($to) || (!$from->isValid() && !$to->isValid())) {
                return array('from' => $this->getPost(true), 'to' => new Date());
            }

            return array('from' => $from, 'to' => $to);
        } elseif (get($this->name)) {
            if (strpos(get($this->name), ' - ')) {
                $pieces = explode(' - ', get($this->name));
                $from = new Date($pieces[0]);
                $to = new Date($pieces[1]);
            }
            if (!isset($from) || !isset($to) || (!$from->isValid() && !$to->isValid())) {
                return array('from' => $this->getPost(true), 'to' => new Date());
            }

            return array('from' => $from, 'to' => $to);
        } elseif (
            post($this->name . '-from-year')
            && post($this->name . '-from-month')
            && post($this->name . '-from-day')
            && post($this->name . '-to-year') && post($this->name . '-to-month')
            && post($this->name . '-to-day')
        ) {
            $from = new Date(
                post($this->name . '-from-year') . '-'
                . post($this->name . '-from-month') . '-'
                . post($this->name . '-from-day')
            );
            $to = new Date(
                post($this->name . '-to-year') . '-'
                . post($this->name . '-to-month') . '-'
                . post($this->name . '-to-day')
            );

            if (!$from->isValid() && !$to->isValid()) {
                return array('from' => $this->getPost(true), 'to' => new Date());
            }

            return array('from' => $from, 'to' => $to);
        } else {
            return array('from' => new Date(), 'to' => new Date());
        }
    }

    public function getPost($skipRangeCheck = false, $name = null)
    {
        if ($name === null) {
            $name = $this->name;
        }
        if ($this->useRange && !$skipRangeCheck) {
            return $this->getPostRange()['from'];
        }

        if (post($name)) {
            return new Date(post($name));
        } elseif (post($name . '-year') && post($name . '-month')
            && post($name . '-day')) {
            return new Date(post($name . '-year') . '-' .
                post($name . '-month') . '-' .
                post($name . '-day'));
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
            for($i = 1; $i < 32; $i++) {
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
            for($i = $this->fromYear; $i < $this->toYear; $i++) {
                $s->option($i, $i);
            }
            $out .= $s;
        } elseif ($this->view == 'text') {
            $done = false;
            $selected = $this->post ? post($this->name) : get($this->name);
            if ($this->useRange) {
                if ($selected) {
                    $pieces = explode(' - ', $selected);
                    $from = new Date($pieces[0]);
                    $to = new Date($pieces[1]);
                } elseif ($this->date && $this->dateTo) {
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
                if ($selected) {
                    $date = new Date($selected);
                    $selected = $date->getDate(Date::FORMAT_SIMPLIFIED);
                } else {
                    $selected = $this->date ? $this->date->getDate(Date::FORMAT_SIMPLIFIED) : '';
                }
            }
            $out .= '<input type="text" name="' . $this->name . '" value="' . $selected . '" />' . "\n";
        }
        return $out;
    }
}
