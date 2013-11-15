<?php
namespace TKOlomouc\View;

abstract class ViewAbstract
{
    protected $twigEnvironment;

    public function __construct(\Twig_Environment $twig)
    {
        $this->twigEnvironment = $twig;
    }

    abstract function render();

    protected function renderTemplate($filename, array $context)
    {
        $this->twigEnvironment->display($filename, $context);
    }
}
