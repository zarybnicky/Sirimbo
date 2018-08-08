<?php
class SelectHelper
{
    protected $name;
    protected $value;
    protected $options;

    public function select($name = null)
    {
        $this->name = $name ?: '';
        $this->value = null;
        $this->options = [];
        $this->cls = 'form-control';

        return $this;
    }

    public function name($name)
    {
        $this->name = $name;
        return $this;
    }

    public function set($value)
    {
        $this->value = $value;
        return $this;
    }

    public function cls($cls)
    {
        $this->cls = $cls;
        return $this;
    }

    public function option($value, $name = null, $overwrite = false)
    {
        if ($overwrite) {
            $this->options = [];
        }

        if ($name !== null) {
            $this->options[$value] = $name;
        } elseif ($name === null) {
            $this->options[$value] = $value;
        }
        return $this;
    }

    public function options($options = [], $overwrite = false, $literal = false)
    {
        if ($overwrite) {
            $this->options = [];
        }

        foreach ($options as $value => $name) {
            if (!$name) {
                continue;
            }
            if ($literal) {
                $this->options[$name] = $name;
            } else {
                $this->options[$value] = $name;
            }
        }
        return $this;
    }

    public function optionsAssoc($options, $value, $name, $overwrite = false)
    {
        if ($overwrite) {
            $this->options = [];
        }
        foreach ($options as $x) {
            $this->options[$x[$value]] = $x[$name];
        }
        return $this;
    }

    public function render()
    {
        $selected = $this->value;
        $options = array_map(
            function ($value, $name) use ($selected) {
                return new Tag(
                    'option',
                    [
                        'value' => $value,
                        'selected' => (string) $selected === (string) $value
                    ],
                    $name
                );
            },
            array_keys($this->options),
            array_values($this->options)
        );

        return (string) new Tag(
            'select',
            ['name' => $this->name, 'class' => $this->cls],
            $options
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
