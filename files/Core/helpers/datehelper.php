<?php
/*
 * Example:
echo '<form action="" method="post">';
echo $this->_date('2012-12-21')->name('test1')->selectBox(), '<br/>';
echo $this->_date('2000-01-01')->name('test2')->textBox(), '<br/>';
echo $this->_date()->name('test1')->getPost(), '<br/>';
echo $this->_date()->name('test2')->getPost(), '<br/>';
echo '<input type="submit" value="Send" />';
echo '</form>';
*/
class DateHelper
{
    private $_date;
    private $_dateTo;
    private $_view;
    private $_name;
    private $_post;
    private $_fromYear;
    private $_toYear;
    private $_useRange;

    function date($d = null) {
        $this->_defaultValues();

        if ($d && ($this->_setDate($d)) === null)
            $this->_name($d);

        return $this;
    }
    function _defaultValues() {
        $this->_view = 'text';
        $this->_date = null;
        $this->_name = null;
        $this->_post = true;
        $this->_fromYear = ((int) date('Y')) - 75;
        $this->_toYear = ((int) date('Y')) + 5;
        $this->_useRange = false;
    }
    private function _setDate($d) {
        if (!is_a($d, 'Date') && is_string($d)) {
            $d = new Date($d);
        }
        if (!is_a($d, 'Date') || !$d->isValid()) {
            return null;
        } else {
            return $d;
        }
    }
    function setDate($d = null) {
        $this->_date = $this->_setDate($d);
        return $this;
    }
    function setFromDate($d = null) {
        $this->setDate($d);
        return $this;
    }
    function setToDate($d = null) {
        $this->_dateTo = $this->_setDate($d);
        return $this;
    }
    function selectBox() {
        $this->_view = 'select';
        return $this;
    }
    function textBox() {
        $this->_view = 'text';
        return $this;
    }
    function name($name) {
        if ($name)
            $this->_name = $name;
        return $this;
    }
    function post($post) {
        $this->_post = (bool) $post;
        return $this;
    }
    function fromYear($y) {
        if ($y > 1800)
            $this->_fromYear = $y;
        return $this;
    }
    function toYear($y) {
        if ($y < 2200 && $y > $this->_fromYear)
            $this->_toYear = $y;
        return $this;
    }
    function range($b = true) {
        if ($b)
            $this->_useRange = true;
        else
            $this->_useRange = false;
        return $this;
    }
    function getPostRange() {
        if (!$this->_useRange)
            return array('from' => $this->getPost(), 'to' => new Date());

        if (post($this->_name)) {
            if (strpos(post($this->_name), ' - ')) {
                $pieces = explode(' - ', post($this->_name));
                $from = new Date($pieces[0]);
                $to = new Date($pieces[1]);
            }
            if (!isset($from) || !isset($to) || (!$from->isValid() && !$to->isValid()))
                return array('from' => $this->getPost(true), 'to' => new Date());

            return array('from' => $from, 'to' => $to);
        } elseif (
            post($this->_name . '-from-year')
            && post($this->_name . '-from-month')
            && post($this->_name . '-from-day')
            && post($this->_name . '-to-year') && post($this->_name . '-to-month')
            && post($this->_name . '-to-day')
        ) {
            $from = new Date(
                post($this->_name . '-from-year') . '-'
                . post($this->_name . '-from-month') . '-'
                . post($this->_name . '-from-day')
            );
            $to = new Date(
                post($this->_name . '-to-year') . '-'
                . post($this->_name . '-to-month') . '-'
                . post($this->_name . '-to-day')
            );

            if (!$from->isValid() && !$to->isValid()) {
                return array('from' => $this->getPost(true), 'to' => new Date());
            }

            return array('from' => $from, 'to' => $to);
        } else {
            return array('from' => new Date(), 'to' => new Date());
        }
    }
    function getPost($skipRangeCheck = false, $name = null) {
        if ($name === null)
            $name = $this->_name;
        if ($this->_useRange && !$skipRangeCheck)
            return $this->getPostRange()['from'];

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
    function __toString() {
        return $this->render();
    }
    function render() {
        $out = '';

        if ($this->_view == 'select') {//TODO ranged select... ftf
            $s = Helper::get()->select()->get(false);

            $s->name($this->_name . '-day')
                ->value($this->_date ? $this->_date->getDay() : null)
                ->options(array(), true)
                ->option('00', 'Den');
            for($i = 1; $i < 32; $i++)
                $s->option((($i < 10) ? ('0' . $i) : $i), $i);
            $out .= $s;

            $out .= $s->name($this->_name . '-month')
                ->value($this->_date ? $this->_date->getMonth() : null)
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

            $s->name($this->_name . '-year')
                ->value($this->_date ? $this->_date->getYear() : null)
                ->options(array(), true)
                ->option('0000', 'Rok');
            for($i = $this->_fromYear; $i < $this->_toYear; $i++)
                $s->option($i, $i);
            $out .= $s;
        } elseif ($this->_view == 'text') {
            $done = false;
            $selected = $this->_post ? post($this->_name) : get($this->_name);
            if ($this->_useRange) {
                if ($selected) {
                    $pieces = explode(' - ', $selected);
                    $from = new Date($pieces[0]);
                    $to = new Date($pieces[1]);
                } elseif ($this->_date && $this->_dateTo) {
                    $from = $this->_date;
                    $to = $this->_dateTo;
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
                    $selected = $this->_date ? $this->_date->getDate(Date::FORMAT_SIMPLIFIED) : '';
                }
            }

            $out .= '<input type="text" name="' . $this->_name . '" value="' . $selected . '" />' . "\n";
        }
        return $out;
    }
}