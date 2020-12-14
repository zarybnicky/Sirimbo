<?php
namespace Olymp\Controller;

class Oklubu
{
    public static function klubovi()
    {
        new \RenderHelper('files/View/Main/OKlubu/TreneriInt.inc', [
            'header' => 'Kluboví trenéři'
        ]);
    }

    public static function externi()
    {
        new \RenderHelper('files/View/Main/OKlubu/TreneriExt.inc', [
            'header' => 'Externí trenéři'
        ]);
    }

    public static function saly()
    {
        new \RenderHelper('files/View/Main/OKlubu/Saly.inc', [
            'header' => 'Kde trénujeme'
        ]);
    }
}
