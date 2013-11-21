<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\Sidebar;
use TKOlomouc\Model\DBNovinky;

class Member extends ControllerAbstract
{
    public function __construct()
    {
        Permissions::checkError('nastenka', P_VIEW);
    }

    public function view($id = null)
    {
        if (isset($_SESSION['zaplaceno_text'])) {
            $this->redirect()->setMessage($_SESSION['zaplaceno_text']);
        }
        $data = DBNovinky::getLastNovinky(NOVINKY_COUNT);
        foreach ($data as &$row) {
            $new_row = array(
                'id' => $row['no_id'],
                'text' => $row['no_text'],
                'user' => $row['u_jmeno'] . ' ' . $row['u_prijmeni'],
                'timestamp' => formatTimestamp($row['no_timestamp'])
            );
            $row = $new_row;
        }
        $this->render(
            'src/application/View/Member/Home.inc',
            array(
                'data' => $data,
                'canEdit' => Permissions::check('novinky', P_OWNED)
            )
        );
    }

    public function sidebar()
    {
        $s = new Sidebar();

        echo $s->menuHeader();
        echo $s->menuItem('Novinky', '/member/home');
        echo $s->menuItem('Nástěnka', '/member/nastenka');
        echo $s->menuItem('Rozpis tréninků', '/member/rozpis');
        echo $s->menuItem('Nabidka tréninků', '/member/nabidka');
        echo $s->menuItem('Klubové akce', '/member/akce');
        echo $s->menuItem('Dokumenty', '/member/dokumenty');
        echo $s->menuItem('Žebříček', '/member/pary');
        echo $s->menuItem('Přehled členů', '/member/clenove/structure');
        echo $s->menuItem('Profil', '/member/profil');

        echo $s->commonItems();
    }
}
