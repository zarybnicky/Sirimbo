<?php
class ColorSelectHelper
{
    private $_field;
    private $_value;

    function colorselect($field = null, $value = null) {
        $this->_defaultValues();
        if ($field !== null)
            $this->_field = $field;
        if ($value !== null)
            $this->_value = $value;
        return $this;
    }
    private function _defaultValues() {
        $this->_field = '';
        $this->_value = '';
    }
    function field($field) {
        $this->_field = $field;
        return $this;
    }
    function value($value) {
        $this->_value = $value;
        return $this;
    }
    function __toString() {
        return $this->render();
    }
    function render() {
        if (!$this->_value && post($this->_field))
            $this->_value = post($this->_field);

        return
        "<script src='/scripts/spectrum.js'></script>
        <link rel='stylesheet' href='/style/spectrum.css' />
        <input id='color-{$this->_field}' type='color' name='{$this->_field}' value='{$this->_value}' />
        <script>$('#color-{$this->_field}').spectrum({color:'{$this->_value}',showInput:true,
            clickoutFiresChange:true,preferredFormat:'hex6'});</script>";
    }
}