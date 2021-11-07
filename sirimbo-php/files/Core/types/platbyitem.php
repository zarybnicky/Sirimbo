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

    public function __construct($specific, $variable, $date, $amount, $prefix = null, $id = null, $categoryId = null)
    {
        $this->specific = (int) $specific;
        $this->variable = (int) $variable;
        $this->date = (string) new Date($date);
        $this->amount = number_format(floatval(str_replace(',', '.', $amount)), 2, '.', '');
        $this->prefix = (int) $prefix;
        $this->id = (int) $id;
        $this->categoryId = (int) $categoryId;
    }

    public function processWithSymbolLookup($userLookup, $categoryLookup)
    {
        if (!$this->specific && $this->categoryId) {
            $category = \DBPlatbyCategory::getSingle($this->categoryId);
            $this->specific = $category['pc_symbol'] ?? null;
        } elseif ($this->specific && !$this->categoryId) {
            if (mb_strlen($this->specific) <= 4) {
                if (isset($categoryLookup[$this->specific])) {
                    $this->categoryId = $categoryLookup[$this->specific]['pc_id'];
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
                    }
                }
            }
        } else {
            $this->specific = null;
            $this->categoryId = null;
        }
        if (!$this->prefix) {
            $this->prefix = $this->date ? (new Date($this->date))->getYear() : 0;
        }
        if ($this->variable && !isset($userLookup[$this->variable])) {
            $this->variable = null;
        }
        $this->isValid =
            $this->specific && $this->variable && $this->date &&
            $this->amount && $this->prefix && $this->categoryId;
    }
}
