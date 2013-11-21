<?php
namespace TKOlomouc\View\Helper;

use TKOlomouc\View\Partial;

class Select extends Partial
{
    private $file = 'src/library/Template/Helper/Select.tpl';

    private $name    = '';
    private $value   = '';
    private $options = array();
    private $post    = true;

    public function __construct($n = null)
    {
        if ($n !== null) {
            return $this->name($n);
        }
        return $this;
    }

    public function name($name)
    {
        $this->name = $name;

        return $this;
    }

    public function value($value)
    {
        $this->value = $value;

        return $this;
    }

    public function post($post = true)
    {
        $this->post = (bool) $post;

        return $this;
    }

    public function get($get = true)
    {
        $this->post = !(bool) $get;

        return $this;
    }

    public function option($value, $name = null, $overwrite = false)
    {
        if ($overwrite == true) {
            $this->options = array();
        }

        if ($name === null) {
            $name = $value;
        }

        $this->options[$value] = $name;

        return $this;
    }

    public function options(
        array $options,
        $overwrite = false,
        $keyAsValue = true,
        $isAssociative = true
    ) {
        if ($overwrite) {
            $this->options = array();
        }

        if ($keyAsValue == false) {
            $options = array_flip($options);
        }

        foreach ($options as $value => $name) {
            if ($isAssociative) {
                $this->option($value, $name, false);
            } else {
                $this->option($name, $name, false);
            }
        }

        return $this;
    }

    public function __toString()
    {
        return $this->render();
    }

    public function render()
    {
        $selected = $this->post ? post($this->name) : get($this->name);

        if ($selected === null) {
            $selected = $this->value;
        }
        return $this->renderTemplate(
            $this->file,
            array(
        	    'name'     => $this->name,
                'selected' => $selected,
                'options'  => $this->options,
            )
        );
    }
}
