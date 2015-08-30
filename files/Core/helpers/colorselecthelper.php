<?php
class ColorSelectHelper
{
    protected $field;
    protected $value;

    public function colorselect($field, $value = null)
    {
        $this->field = $field;
        $this->value = $value ?: null;

        return $this;
    }

    public function render()
    {
        return
        "<script src='/scripts/spectrum.js'></script>
        <link rel='stylesheet' href='/style/spectrum.css' />
        <input id='color-{$this->field}' type='color' name='{$this->field}' value='{$this->value}' />
        <script>$('#color-{$this->field}').spectrum({color:'{$this->value}',showInput:true,
            clickoutFiresChange:true,preferredFormat:'hex6'});</script>";
    }

    public function __toString()
    {
        return $this->render();
    }
}