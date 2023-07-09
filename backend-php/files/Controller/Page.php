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

    public static function akceSingle($id)
    {
        \Render::twig('Member/AkceSingle.twig', ['id' => $id]);
    }

    public static function akce()
    {
        \Render::twig('Member/Akce.twig');
    }

    public static function schedule()
    {
        \Render::twig('Member/Schedule.twig');
    }
}
