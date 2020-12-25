<?php
namespace Olymp\Controller;

class StaticContent
{
    public static function kontakt()
    {
        \Render::page('files/View/Main/Kontakt.inc', [
            'header' => 'Kontakt'
        ]);
    }

    public static function klubovi()
    {
        \Render::page('files/View/Main/OKlubu/TreneriInt.inc', [
            'header' => 'Kluboví trenéři'
        ]);
    }

    public static function externi()
    {
        \Render::page('files/View/Main/OKlubu/TreneriExt.inc', [
            'header' => 'Externí trenéři'
        ]);
    }

    public static function saly()
    {
        \Render::page('files/View/Main/OKlubu/Saly.inc', [
            'header' => 'Kde trénujeme'
        ]);
    }
}
