<?php
class CheckboxHelper {
    protected $name;
    protected $value;
    protected $state;
    protected $readonly;

    public function checkbox($name, $value = null)
    {
        if ($value === null) {
            $value = $name;
        }

        $this->name = $name;
        $this->value = $value;

        $this->state = false;
        $this->readonly = false;

        return $this;
    }

    public function set($val)
    {
        $this->state = (bool) $val;
        return $this;
    }

    public function readonly($val)
    {
        $this->readonly = $val;
        return $this;
    }

    public function render()
    {
        return
            '<input type="checkbox"' .
            " name=\"{$this->name}\" " .
            " value=\"{$this->value}\"" .
            ($this->state ? ' checked="checked"' : '') .
            ($this->readonly ? ' readonly="readonly"' : '') .
            '/>';
    }

    public function __toString()
    {
        return $this->render();
    }
}
