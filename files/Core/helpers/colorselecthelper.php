<?php
class ColorSelectHelper
{
    private $field;
    private $value;

    public function colorselect($field = null, $value = null) {
        $this->_defaultValues();
        if ($field !== null)
            $this->field = $field;
        if ($value !== null)
            $this->value = $value;
        return $this;
    }
    private function _defaultValues() {
        $this->field = '';
        $this->value = '';
    }
    public function field($field) {
        $this->field = $field;
        return $this;
    }
    public function set($value) {
        $this->value = $value;
        return $this;
    }
    public function __toString() {
        return $this->render();
    }
    public function render() {
        return
        "<script src='/scripts/spectrum.js'></script>
        <link rel='stylesheet' href='/style/spectrum.css' />
        <input id='color-{$this->field}' type='color' name='{$this->field}' value='{$this->value}' />
        <script>$('#color-{$this->field}').spectrum({color:'{$this->value}',showInput:true,
            clickoutFiresChange:true,preferredFormat:'hex6'});</script>";
    }
}