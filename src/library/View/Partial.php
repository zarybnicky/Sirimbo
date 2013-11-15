<?php
namespace TKOlomouc\View;

abstract class Partial extends ViewAbstract
{
    protected function renderTemplate($filename, array $context)
    {
        return $this->twigEnvironment->render($filename, $context);
    }
}