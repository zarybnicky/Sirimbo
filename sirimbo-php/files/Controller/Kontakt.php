<?php
namespace Olymp\Controller;

class Kontakt
{
    public static function get()
    {
        \Render::page('files/View/Main/Kontakt.inc', [
            'header' => 'Kontakt'
        ]);
    }
}
