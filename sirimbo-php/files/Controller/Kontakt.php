<?php
namespace Olymp\Controller;

class Kontakt
{
    public static function get()
    {
        new \RenderHelper('files/View/Main/Kontakt.inc', [
            'header' => 'Kontakt'
        ]);
    }
}
