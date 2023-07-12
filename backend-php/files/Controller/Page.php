<?php
namespace Olymp\Controller;

class Page
{
    public static function prijdTancit()
    {
        \Render::twig('CustomElement.twig', [
            'title' => "Přijď tančit!",
            'content' => '<prijd-tancit></prijd-tancit>',
        ]);
    }

    public static function ochranaUdaju()
    {
        \Render::twig('Main/OchranaUdaju.twig');
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

    public static function kontakt()
    {
        \Render::twig('CustomElement.twig', [
            'title' => 'Kontakt',
            'content' => '<olymp-contact></olymp-contact>'
        ]);
    }

    public static function articles()
    {
        \Render::twig('CustomElement.twig', [
            'title' => 'Články',
            'content' => '<olymp-articles></olymp-articles>'
        ]);
    }
    public static function akceSingle($id)
    {
        \Render::twig('CustomElement.twig', [
            'title' => 'Klubové akce',
            'content' => "<event-member-list selected='$id'></event-member-list><event-item id='$id'></event-item>",
        ]);
    }

    public static function akce()
    {
        \Render::twig('CustomElement.twig', [
            'title' => 'Klubové akce',
            'content' => '<event-member-list></event-member-list>',
        ]);
    }

    public static function schedule()
    {
        \Render::twig('CustomElement.twig', [
            'title' => 'Tréninky',
            'content' => '<schedule-view></schedule-view>',
        ]);
    }

    public static function nastenka()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        \Render::twig('CustomElement.twig', [
            'title' => 'Nástěnka',
            'content' => '<announcement-list style="flex:1"></announcement-list>',
        ]);
    }

    public static function paryList()
    {
        \Permissions::checkError('pary', P_OWNED);
        \Render::twig('CustomElement.twig', [
            'title' => "Správa párů",
            'content' => '<couple-admin-list style="display:flex;flex-direction:column;flex:1"></couple-admin-list>',
        ]);
    }

    public static function parySingle($id)
    {
        \Permissions::checkError('pary', P_OWNED);
        \Render::twig('CustomElement.twig', [
            'title' => 'Správa párů',
            'content' => "<couple-view id='$id'></couple-view>"
        ]);
    }
}
