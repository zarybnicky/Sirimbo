<?php
namespace Olymp\Controller\Admin;

class Pary
{
    public static function list()
    {
        \Permissions::checkError('pary', P_OWNED);
        \Render::twig('Admin/Pary.twig', [
            'header' => 'Správa párů',
            'data' => \DBPary::getActivePary(),
            'usersMen' => \DBUser::getUsersByPohlavi('m'),
            'usersWomen' => \DBUser::getUsersByPohlavi('f')
        ]);
    }

    public static function listPost()
    {
        \Permissions::checkError('pary', P_OWNED);
        switch ($_POST["action"]) {
            case 'add':
                if ($_POST["add_partner"]) {
                    \DBPary::newCouple($_POST["add_partner"], $_POST["add_partnerka"]);
                }
                break;
            case 'fix_unpaired':
                $xs = \DBPary::getUnpairedUsers();
                foreach ($xs as $x) {
                    \DBPary::noPartner($x['u_id']);
                }
                \Message::info(count($xs) . ' chybných záznamů opraveno');
                break;
        }
        \Redirect::to('/admin/pary');
    }

    public static function remove($id)
    {
        \Permissions::checkError('pary', P_OWNED);
        \DBPary::removeCouple($id);
        \Message::success('Pár odstraněn');
        \Redirect::to('/admin/pary');
    }
}
