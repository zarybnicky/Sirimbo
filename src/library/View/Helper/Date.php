<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\View\Partial;
use TKOlomouc\Type\DateFormat;

/*
 * Example:
 * echo '<form action="" method="post">';
 * echo $date->setDate('2012-12-21')->name('test1')->selectBox(), '<br/>';
 * echo date->setDate('2000-01-01')->name('test2')->textBox(), '<br/>';
 * echo date->setDate()->name('test1')->getPost(), '<br/>';
 * echo $date->setDate()->name('test2')->getPost(), '<br/>';
 * echo '<input type="submit" value="Send" />';
 * echo '</form>';
*/
class Date extends Partial
{
    //TODO: Split into Helper\Date\Simple and Helper\Date\Range (with Helper\Date as parent?)

    private $name        = null;
    private $date        = null;
    private $dateTo      = null;
    private $fromYear    = null;
    private $toYear      = null;
    private $isDateRange = false;
    private $isPost      = true;
    private $displayType = 'text';

    public function __construct($d = null)
    {
        $this->fromYear((int) date('Y') - 75);
        $this->toYear((int) date('Y') + 5);

        if ($d !== null && $this->setDate($d) === null) {
            $this->name($d);
        }
    }

    private function updateDate(&$var, $d)
    {
        if (!is_a($d, 'DateFormat')) {
            $d = new DateFormat($d);
        }

        if (!$d->isValid()) {
            return null;
        }

        $var = $d;
        return $d;
    }

    public function setDate($d = null)
    {
        $this->updateDate($this->date, $d);

        return $this;
    }

    public function setFromDate($d = null)
    {
        $this->updateDate($this->date, $d);

        return $this;
    }

    public function setToDate($d = null)
    {
        $this->updateDate($this->dateTo, $d);

        return $this;
    }

    public function selectBox()
    {
        $this->displayType = 'select';

        return $this;
    }

    public function textBox()
    {
        $this->displayType = 'text';

        return $this;
    }

    public function name($name)
    {
        $this->name = $name;

        return $this;
    }

    public function post($post)
    {
        $this->isPost = (bool) $post;

        return $this;
    }

    public function fromYear($year)
    {
        $this->fromYear = $year;

        return $this;
    }

    public function toYear($year)
    {
        if ($year < $this->fromYear) {
            $this->toYear = $this->fromYear;
            return $this;
        }

        $this->toYear = $year;

        return $this;
    }

    public function range($useRange = true)
    {
        $this->isDateRange = (bool) $useRange;

        return $this;
    }
    public function getPostRange()
    {
        if (!$this->isDateRange) {
            return array('from' => $this->getPost(), 'to' => new DateFormat());
        }

        if (post($this->name)) {
            if (strpos(post($this->name), ' - ')) {
                $pieces = explode(' - ', post($this->name));
                $from = new DateFormat($pieces[0]);
                $to = new DateFormat($pieces[1]);
            }
            if (!isset($from) || !isset($to) || (!$from->isValid() && !$to->isValid())) {
                return array('from' => $this->getPost(true), 'to' => new DateFormat());
            }

            return array('from' => $from, 'to' => $to);
        } elseif (
            post($this->name . '-from-year')
            && post($this->name . '-from-month')
            && post($this->name . '-from-day')
            && post($this->name . '-to-year')
            && post($this->name . '-to-month')
            && post($this->name . '-to-day')
        ) {
            $from = new DateFormat(
                post($this->name . '-from-year') . '-'
                . post($this->name . '-from-month') . '-'
                . post($this->name . '-from-day')
            );
            $to = new DateFormat(
                post($this->name . '-to-year') . '-'
                . post($this->name . '-to-month') . '-'
                . post($this->name . '-to-day')
            );

            if (!$from->isValid() && !$to->isValid()) {
                return array('from' => $this->getPost(true), 'to' => new DateFormat());
            }

            return array('from' => $from, 'to' => $to);
        } else {
            return array('from' => new DateFormat(), 'to' => new DateFormat());
        }
    }

    public function getPost($skipRangeCheck = false, $name = null)
    {
        if ($name === null) {
            $name = $this->name;
        }

        if ($this->isDateRange && !$skipRangeCheck) {
            return $this->getPostRange()['from'];
        }

        if (post($name)) {
            return new DateFormat(post($name));
        } elseif (post($name . '-year') && post($name . '-month')
            && post($name . '-day')) {
            return new DateFormat(
                post($name . '-year') . '-'
                . post($name . '-month') . '-'
                . post($name . '-day')
            );
        } else {
            return new DateFormat();
        }
    }

    private function renderSelect()
    {
        $select = new Select();
        $select->post();

        //Day select
        $select->name($this->name . '-day')
            ->value($this->date ? $this->date->getDay() : null)
            ->option('00', 'Den', true);

        for ($i = 1; $i < 32; $i++) {
            $select->option(
                ($i < 10) ? ('0' . $i) : $i,
                $i . '.'
            );
        }
        $out .= $select->render();

        //Month select
        $out .= $select->name($this->name . '-month')
            ->value($this->date ? $this->date->getMonth() : null)
            ->option('00', 'Měsíc', true)
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

        $out .= $select->render();

        //Year select
        $select->name($this->name . '-year')
            ->value($this->date ? $this->date->getYear() : null)
            ->option('0000', 'Rok', true);

        for ($i = $this->fromYear; $i < $this->toYear; $i++) {
            $select->option($i, $i);
        }
        return $out . $select->render();
    }

    private function renderTextbox()
    {
        $done = false;
        $selected = $this->isPost ? post($this->name) : get($this->name);

        if ($this->isDateRange) {
            if ($selected) {
                $pieces = explode(' - ', $selected);
                $from = new DateFormat($pieces[0]);
                $to = new DateFormat($pieces[1]);
            } elseif ($this->date && $this->dateTo) {
                $from = $this->date;
                $to = $this->dateTo;
            } else {
                $from = new DateFormat();
                $to = new DateFormat();
            }
            if ($from->isValid() || $from->isValid()) {
                $selected =
                    $from->getDate(DateFormat::FORMAT_SIMPLIFIED) . ' - '
                    . $to->getDate(DateFormat::FORMAT_SIMPLIFIED);
                $done = true;
            }
        }
        if (!$done) {
            if ($selected) {
                $date = new DateFormat($selected);
                $selected = $date->getDate(DateFormat::FORMAT_SIMPLIFIED);
            } else {
                $selected = $this->date ? $this->date->getDate(DateFormat::FORMAT_SIMPLIFIED) : '';
            }
        }
        return "<input type='text' name='{$this->name}' value='{$selected}' />\n";
    }

    public function __toString()
    {
        return $this->render();
    }

    public function render()
    {
        //TODO: Utilize templates for <select>
        //TODO: ranged date select!
        if ($this->displayType == 'select') {
            return $this->renderSelect();
        }
        if ($this->displayType == 'text') {
            return $this->renderTextbox();
        }
        return '';
    }
}