<?php
namespace Olymp\Controller;

class Page
{
    public static function prijdTancit()
    {
        \Render::twig('Main/PrijdTancit.twig');
    }

    public static function ochranaUdaju()
    {
        \Render::twig('Main/OchranaUdaju.twig');
    }
}
