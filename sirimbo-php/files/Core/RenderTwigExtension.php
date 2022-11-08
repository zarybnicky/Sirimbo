<?php

use \Twig\TwigFilter;
use \Twig\TwigFunction;

class RenderTwigExtension extends \Twig\Extension\AbstractExtension
{
    public function getFunctions()
    {
        return [
            new TwigFunction('text', '\\Utils::text', ['is_safe' => ['html']]),
            new TwigFunction('radio', '\\Utils::radio', ['is_safe' => ['html']]),
            new TwigFunction('select', '\\Utils::select', ['is_safe' => ['html']]),
            new TwigFunction('checkbox', '\\Utils::checkbox', ['is_safe' => ['html']]),
            new TwigFunction('dateRange', '\\Utils::dateRange', ['is_safe' => ['html']]),
            new TwigFunction('userSelect', '\\Utils::userSelect', ['is_safe' => ['html']]),
            new TwigFunction('inlineRadio', '\\Utils::inlineRadio', ['is_safe' => ['html']]),
            new TwigFunction('selectAssoc', '\\Utils::selectAssoc', ['is_safe' => ['html']]),
            new TwigFunction('partnerRequest', '\\Utils::partnerRequest', ['is_safe' => ['html']]),
            new TwigFunction('getMessages', '\\Message::get'),
            new TwigFunction('json_encode', 'json_encode'),
        ];
    }
}
