<?php
class RenderTwigExtension extends \Twig\Extension\AbstractExtension
{
    public function getFunctions()
    {
        return [
            new \Twig\TwigFunction('text', '\\Utils::text', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('date', '\\Utils::date', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('radio', '\\Utils::radio', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('table', '\\Utils::table', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('notice', '\\Utils::notice', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('select', '\\Utils::select', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('checkbox', '\\Utils::checkbox', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('dateRange', '\\Utils::dateRange', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('navbarItem', '\\Utils::navbarItem', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('userSelect', '\\Utils::userSelect', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('inlineRadio', '\\Utils::inlineRadio', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('selectAssoc', '\\Utils::selectAssoc', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('selectLiteral', '\\Utils::selectLiteral', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('partnerRequest', '\\Utils::partnerRequest', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('timestamp', '\\Format::timestamp'),
            new \Twig\TwigFunction('getMessages', '\\Message::get'),
        ];
    }
}
