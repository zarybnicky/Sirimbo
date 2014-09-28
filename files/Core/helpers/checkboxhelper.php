<?php
class CheckboxHelper {
    protected $name;
    protected $value;
    protected $defaultState;
    protected $get;
    protected $readonly;

    public function checkbox($name, $value = null) {
        if ($value === null) {
            $value = $name;
        }

        $this->name = $name;
        $this->value = $value;

        $this->defaultState = false;
        $this->get = false;
        $this->readonly = false;

        return $this;
    }

    public function defaultState($val) {
        $this->defaultState = $val;
        return $this;
    }

    public function get($val) {
        $this->get = $val;
        return $this;
    }

    public function readonly($val) {
        $this->readonly = $val;
        return $this;
    }
    
    public function render() {
        $checked =
            $this->defaultState ||
            (($this->get == true) ?
             (get($this->name) != false) :
             (post($this->name) != false));

        return
            '<input type="checkbox"' .
            " name=\"{$this->name}\" " .
            " value=\"{$this->value}\"" .
            ($checked ? ' checked="checked"' : '') .
            ($this->readonly ? ' readonly="readonly"' : '') .
            '/>';
    }

    public function __toString() {
        return $this->render();
    }
}
