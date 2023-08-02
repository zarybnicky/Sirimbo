<?php
namespace Olymp\Controller\Admin;

class Nabidka
{
    public static function list()
    {
        \Permissions::checkError('nabidka', P_OWNED);
        \Render::twig('Admin/Nabidka.twig');
    }

    public static function edit($id)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM nabidka WHERE n_id='?'", $id);
        if (!$data) {
            \Message::warning('Nabídka s takovým ID neexistuje');
            \Redirect::to('/admin/nabidka');
        }
        \Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);
        return self::displayForm('edit', $data);
    }

    protected static function displayForm($action, $data = [])
    {
        \Render::twig('Admin/NabidkaForm.twig', [
            'action' => $action,
            'returnURI' => $_SERVER['HTTP_REFERER'],
            'users' => \Permissions::check('nabidka', P_ADMIN)
            ? \DBUser::getUsersByPermission('nabidka', P_OWNED)
            : [\DBUser::getUserData(\Session::getUser()->getId())],
            'id' => $data['n_id'] ?? null,
            'trener' => $_POST['trener'] ?? $data['n_trener'] ?? '',
            'pocet_hod' => $_POST['pocet_hod'] ?? $data['n_pocet_hod'] ?? '',
            'max_pocet_hod' => $_POST['max_pocet_hod'] ?? $data['n_max_pocet_hod'] ?? '',
            'od' => $_POST['od'] ?? $data['n_od'] ?? '',
            'do' => $_POST['do'] ?? $data['n_do'] ?? '',
            'visible' => $_POST['visible'] ?? $data['n_visible'] ?? false,
            'lock' => $_POST['lock'] ?? $data['n_lock'] ?? ''
        ]);
    }
}
