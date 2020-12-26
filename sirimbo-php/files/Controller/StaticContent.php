<?php
namespace Olymp\Controller;

class StaticContent
{
    public static function kontakt()
    {
        \Render::twig('Main/Kontakt.twig', [
            'header' => 'Kontakt'
        ]);
    }

    public static function klubovi()
    {
        \Render::twig('Main/TreneriInterni.twig', [
            'header' => 'Kluboví trenéři'
        ]);
    }

    public static function externi()
    {
        \Render::twig('Main/TreneriExterni.twig', [
            'header' => 'Externí trenéři'
        ]);
    }

    public static function saly()
    {
        \Render::twig('Main/Saly.twig', [
            'header' => 'Kde trénujeme'
        ]);
    }
}
