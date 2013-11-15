<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\View\Partial;

class ColorSelect extends Partial
{
    private final $file = 'Helper/ColorSelect.tpl';

    private $field = '';
    private $value = '';

    public function __construct($twig, $field = null, $value = null)
    {
        parent::__construct($twig);

        if ($field !== null) {
            $this->field = $field;
        }
        if ($value !== null) {
            $this->value = $value;
        }
    }

    public function field($field)
    {
        $this->field = $field;

        return $this;
    }

    public function value($value)
    {
        $this->value = $value;

        return $this;
    }

    public function __toString()
    {
        return $this->render();
    }

    public function render() {
        if (!$this->value && post($this->field)) {
            $this->value = post($this->field);
        }
        return $this->renderTemplate(
            $this->file,
            array(
        	    'field' => $this->field,
                'value' => $this->value
            )
        );
    }
}