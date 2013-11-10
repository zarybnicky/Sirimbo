<?php
namespace TKOlomouc\View;

abstract class Partial
{
    protected function renderTemplate($filename, array $context)
    {
        return $this->twigEnvironment->render($filename, $context);
    }
}