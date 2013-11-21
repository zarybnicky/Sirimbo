<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\Sidebar;

class Admin extends ControllerAbstract
{
    public function __construct()
    {
        Permissions::checkError('nastenka', P_OWNED);
    }
    public function view($id = null)
    {
        $this->render('src/application/View/Admin/Home.inc');
    }
    public function sidebar()
    {
        $sidebar = new Sidebar();

        echo $sidebar->menuHeader();
        echo $sidebar->menuItem(
            'Správa uživatelů',
            '/admin/users',
            'users',
            P_OWNED
        );
        echo $sidebar->menuItem(
            'Správa skupin',
            '/admin/skupiny',
            'skupiny',
            P_OWNED
        );
        echo $sidebar->menuItem(
            'Správa plateb',
            '/admin/platby',
            'platby',
            P_OWNED
        );
        echo $sidebar->menuItem(
            'Správa párů',
            '/admin/pary',
            'pary',
            P_OWNED
        );
        echo $sidebar->menuItem(
            'Správa článků',
            '/admin/aktuality',
            'aktuality',
            P_OWNED
        );
        echo $sidebar->menuItem(
            'Správa nástěnky',
            '/admin/nastenka',
            'nastenka',
            P_OWNED
        );
        echo $sidebar->menuItem(
            'Správa rozpisů',
            '/admin/rozpis',
            'rozpis',
            P_OWNED
        );
        echo $sidebar->menuItem(
            'Správa nabídky',
            '/admin/nabidka',
            'nabidka',
            P_OWNED
        );
        echo $sidebar->menuItem(
            'Správa akcí',
            '/admin/akce',
            'akce',
            P_OWNED
        );
        echo $sidebar->menuItem(
            'Správa galerie',
            '/admin/galerie',
            'galerie',
            P_OWNED
        );
        echo $sidebar->menuItem(
            'Správa dokumentů',
            '/admin/dokumenty',
            'dokumenty',
            P_OWNED
        );
        echo $sidebar->menuItem(
            'Správa anket',
            '/admin/ankety',
            'ankety',
            P_OWNED
        );
        echo $sidebar->menuItem(
            'Správa oprávnění',
            '/admin/permissions',
            'permissions',
            P_OWNED
        );
        echo $sidebar->menuItem(
            'Konzole',
            '/admin/konzole',
            'konzole',
            P_OWNED
        );

        echo $sidebar->commonItems();
    }
}
