<?php
namespace Olymp\Controller;

class StaticContent
{
    public static function kontakt()
    {
        \Render::twig('Main/Kontakt.twig');
    }

    public static function klubovi()
    {
        \Render::twig('Main/TreneriInterni.twig');
    }

    public static function externi()
    {
        \Render::twig('Main/TreneriExterni.twig');
    }

    public static function saly()
    {
        \Render::twig('Main/Saly.twig');
    }
}
