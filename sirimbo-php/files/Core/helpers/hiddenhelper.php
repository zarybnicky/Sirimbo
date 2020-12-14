<?php
class HiddenHelper
{
    protected $name;
    protected $value;

    public function __construct($name, $value = null)
    {
        if ($value === null) {
            $value = $name;
        }

        $this->name = $name;
        $this->value = $value;
    }

    public function __toString()
    {
        return (string) new Tag(
            'input',
            ['type' => 'hidden', 'name' => $this->name, 'value' => $this->value]
        );
    }
}
