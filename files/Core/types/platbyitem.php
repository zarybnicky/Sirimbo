<?php
class PlatbyItem
{
    public $specific;
    public $variable;
    public $date;
    public $amount;
    public $prefix;
    public $id;
    public $categoryId;
    public $isValid = false;

    public function __construct()
    {
    }

    public function init($specific, $variable, $date, $amount, $prefix = null, $id = null, $categoryId = null)
    {
        $this->_initFormatted($specific, $variable, $date, $amount, $prefix, $id, $categoryId);
        $this->isValid = false;
        return $this;
    }

    private function _initFormatted($specific, $variable, $date, $amount, $prefix, $id, $categoryId)
    {
        $this->specific = (int) $specific;
        $this->variable = (int) $variable;
        $this->date = (string) new Date($date);
        $amount = floatval(str_replace(',', '.', $amount));
        $this->amount = is_float($amount) ? number_format($amount, 2, '.', '') : '0.00';
        $this->prefix = (int) $prefix;
        $this->id = (int) $id;
        $this->categoryId = (int) $categoryId;
    }

    public function processWithSymbolLookup($userLookup, $categoryLookup)
    {
        if (!$this->specific && $this->categoryId) {
            $category = DBPlatbyCategory::getSingle($this->categoryId);
            $this->specific = $category ? $category['pc_symbol'] : null;
        } elseif ($this->specific && !$this->categoryId) {
            if (mb_strlen($this->specific) <= 4) {
                if (isset($categoryLookup[$this->specific])) {
                    $this->categoryId = $categoryLookup[$this->specific]['pc_id'];
                }
                if (!$this->prefix) {
                    $this->prefix = $this->date ? (new Date($this->date))->getYear() : 0;
                }
            } else {
                $specific = $this->specific;
                if (isset($categoryLookup[$specific])) {
                    $this->categoryId = $categoryLookup[$specific]['pc_id'];
                } else {
                    $specific = substr($specific, 4);
                    if (isset($categoryLookup[$specific]) && $categoryLookup[$specific]['pc_use_prefix']) {
                        $this->categoryId = $categoryLookup[$specific]['pc_id'];
                    }
                }
                if (!$this->prefix) {
                    if (isset($categoryLookup[$specific]) && $categoryLookup[$specific]['pc_use_prefix'] && strlen($this->specific) >= 4) {
                        $this->prefix = substr($this->specific, 0, 4);
                    } else {
                        $this->prefix = $this->date ? (new Date($this->date))->getYear() : 0;
                    }
                }
            }
        } else {
            $this->specific = null;
            $this->categoryId = null;
        }
        if ($this->variable && !isset($userLookup[$this->variable])) {
            $this->variable = null;
        }
        $this->isValid =
            $this->specific && $this->variable && $this->date &&
            $this->amount && $this->prefix && $this->categoryId;
    }
}