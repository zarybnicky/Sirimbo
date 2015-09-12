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
        $this->options = array();

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

    public function option($value, $name = null, $overwrite = false)
    {
        if ($overwrite) {
            $this->options = array();
        }

        if ($name !== null) {
            $this->options[$value] = $name;
        } elseif ($name === null) {
            $this->options[$value] = $value;
        }
        return $this;
    }

    public function options($options = array(), $overwrite = false, $literal = false)
    {
        if ($overwrite) {
            $this->options = array();
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

    public function render()
    {
        $selected = $this->value;
        $options = array_map(
            function ($value, $name) use ($selected) {
                return (
                    '<option value="' . $value . '"'
                    . ((string) $selected === (string) $value ? ' selected="selected"' : '')
                    . ">$name</option>\n"
                );
            },
            array_keys($this->options),
            array_values($this->options)
        );

        return (
            '<select name="' . $this->name . '">' . "\n"
            . implode('', $options)
            . "</select>\n"
        );
    }

    public function __toString()
    {
        return $this->render();
    }
}
