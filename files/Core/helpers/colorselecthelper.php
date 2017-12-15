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
            [
                'id' => 'color-' . $this->field,
                'type' => 'color',
                'name' => $this->field,
                'value' => $this->value
            ]
        );
        $widget->addStylesheet(['src' => '/style/spectrum.css']);
        $widget->addScript(['src' => '/scripts/spectrum.js']);
        $widget->addScript(
            [],
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
