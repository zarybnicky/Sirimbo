<?php
namespace Olymp\Controller;

class Page
{
    public static function error()
    {
        \Render::twig('Error.twig', ['errorCode' => $_GET['id']]);
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
            'content' => "<event-item id='$id'></event-item><event-member-list selected='$id'></event-member-list>",
        ]);
    }

    public static function akce()
    {
        \Render::twig('CustomElement.twig', [
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
            'content' => '<announcement-list style="display:block;position:relative"></announcement-list>',
        ]);
    }

    public static function userList()
    {
        \Permissions::checkError('users', P_ADMIN);
        \Render::twig('CustomElement.twig', [
            'title' => 'Správa uživatelů',
            'content' => '<user-list style="display:block;position:relative;height:80vh"></user-list>',
        ]);
    }

    public static function paryList()
    {
        \Permissions::checkError('pary', P_OWNED);
        \Render::twig('CustomElement.twig', [
            'title' => "Správa párů",
            'content' => '<couple-admin-list style="display:block;position:relative;height:80vh"></couple-admin-list>',
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