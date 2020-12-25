<?php
class RenderTwigExtension extends \Twig\Extension\AbstractExtension
{
    public function getFunctions()
    {
        return [
            new \Twig\TwigFunction('notice', '\\Utils::notice', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('navbarItem', '\\Utils::navbarItem', ['is_safe' => ['html']]),
            new \Twig\TwigFunction('getMessages', '\\Message::get'),
        ];
    }
}
