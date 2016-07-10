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
        $widget = new Tag(
            'input',
            array(
                'id' => 'color-' . $this->field,
                'type' => 'color',
                'name' => $this->field,
                'value' => $this->value
            )
        );
        $widget->addStylesheet(array('src' => '/style/spectrum.css'));
        $widget->addScript(array('src' => '/scripts/spectrum.js'));
        $widget->addScript(
            array(),
            <<<EOS
$('#color-{$this->field}').spectrum({
  color:'{$this->value}',
  showInput:true,
  clickoutFiresChange:true,
  preferredFormat:'hex6'
});
EOS
        );
        return (string) $widget;
    }

    public function __toString()
    {
        return $this->render();
    }
}
