<?php
namespace TKOlomouc;

class Settings
{
    public static $documentTypes = array(
        '1'        => 'Schůze, rady',
        '2'        => 'Soutěže',
        '3'        => 'Tábory',
        '0'        => 'Ostatní'
    );

    public static $permissionLevels = array(
        P_NONE => 'Bez přístupu',
        P_VIEW => 'Zobrazit',
        P_MEMBER => 'Editovat',
        P_OWNED => 'Admin (svoje)',
        P_ADMIN => 'Admin'
    );

    public static $permissions = array(
        'akce' => array(
            'name' => 'Akce',
            'default' => P_MEMBER,
            P_NONE => 1,
            P_VIEW => 1,
            P_MEMBER => 1,
            P_OWNED => 1,
            P_ADMIN => 1
        ),
        'aktuality' => array(
            'name' => 'Aktuality',
            'default' => P_VIEW,
            P_VIEW => 1,
            P_OWNED => 1,
            P_ADMIN => 1
        ),
        'ankety' => array(
            'name' => 'Ankety',
            'default' => P_VIEW,
            P_VIEW => 1,
            P_OWNED => 1,
            P_ADMIN => 1
        ),
        'dokumenty' => array(
            'name' => 'Dokumenty',
            'default' => P_MEMBER,
            P_NONE => 1,
            P_MEMBER => 1,
            P_OWNED => 1,
            P_ADMIN => 1
        ),
        'galerie' => array(
            'name' => 'Fotogalerie',
            'default' => P_VIEW,
            P_VIEW => 1,
            P_OWNED => 1,
            P_ADMIN => 1
        ),
        'konzole' => array(
            'name' => 'Konzole',
            'default' => P_NONE,
            P_NONE => 1,
            P_ADMIN => 1
        ),
        'nabidka' => array(
            'name' => 'Nabídka',
            'default' => P_MEMBER,
            P_NONE => 1,
            P_VIEW => 1,
            P_MEMBER => 1,
            P_OWNED => 1,
            P_ADMIN => 1
        ),
        'nastenka' => array(
            'name' => 'Nástěnka',
            'default' => P_VIEW,
            P_NONE => 1,
            P_VIEW => 1,
            P_OWNED => 1,
            P_ADMIN => 1
        ),
        'novinky' => array(
            'name' => 'Novinky',
            'default' => P_VIEW,
            P_NONE => 1,
            P_VIEW => 1,
            P_OWNED => 1,
            P_ADMIN => 1
        ),
        'pary' => array(
            'name' => 'Páry',
            'default' => P_VIEW,
            P_NONE => 1,
            P_VIEW => 1,
            P_ADMIN => 1
        ),
        'platby' => array(
            'name' => 'Platby',
            'default' => P_NONE,
            P_NONE => 1,
            P_ADMIN => 1
        ),
        'permissions' => array(
            'name' => 'Oprávnění',
            'default' => P_NONE,
            P_NONE => 1,
            P_ADMIN => 1
        ),
        'rozpis' => array(
            'name' => 'Rozpis',
            'default' => P_MEMBER,
            P_NONE => 1,
            P_VIEW => 1,
            P_MEMBER => 1,
            P_OWNED => 1,
            P_ADMIN => 1
        ),
        'skupiny' => array(
            'name' => 'Skupiny',
            'default' => P_VIEW,
            P_NONE => 1,
            P_VIEW => 1,
            P_ADMIN => 1
        ),
        'users' => array(
            'name' => 'Uživatelé',
            'default' => P_VIEW,
            P_NONE => 1,
            P_VIEW => 1,
            P_ADMIN => 1
        ),
        'main' => array(
            'name' => 'Veřejná část',
            'default' => P_VIEW,
            P_VIEW => 1
        )
    );
    public static $fotoTypes = array(
        'image/pjpeg' => 'jpg',
        'image/jpeg' => 'jpg',
        'image/gif' => 'gif',
        'image/bmp' => 'bmp',
        'image/x-png' => 'png'
    );
    public static $gdFunctionSuffix = array(
        'image/pjpeg' => 'JPEG',
        'image/jpeg' => 'JPEG',
        'image/gif' => 'GIF',
        'image/bmp' => 'BMP',
        'image/x-png' => 'PNG'
    );
}
