<?php
namespace TKOlomouc\View;

abstract class Partial extends ViewAbstract
{
    protected function renderTemplate($filename, array $context)
    {
        return parent::renderTemplate($filename, $context);
    }
}