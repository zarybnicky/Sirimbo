<?php
namespace Olymp\Controller\Admin;

class RozpisDetail
{
    public static function detail($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        $data = \Database::querySingle(
            "SELECT r_id,r_trener,u_jmeno,u_prijmeni,r_kde,r_datum,r_visible,r_lock FROM rozpis LEFT JOIN users ON r_trener=u_id WHERE r_id='?'",
            $id,
        );
        if (!$data) {
            \Message::warning('Rozpis s takovým ID neexistuje');
            \Redirect::to('/admin/rozpis');
        }
        \Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);
        $items = \DBRozpis::getLessons($id);
        $users = \DBPary::getPartners(array_filter(array_column($items, 'p_id')));
        $data = $data + [
            'canEdit' => \Permissions::check('nabidka', P_OWNED, $data['r_trener'])
        ];

        \Render::twig('Admin/RozpisDetail.twig', [
            'data' => $data,
            'users' => $users,
            'items' => $items
        ]);
    }

    public static function detailPost($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        $data = \Database::querySingle("SELECT r_id,r_trener,r_kde,r_datum,r_visible,r_lock FROM rozpis WHERE r_id='?'", $id);
        if (!$data) {
            \Message::warning('Rozpis s takovým ID neexistuje');
            \Redirect::to('/admin/rozpis');
        }
        \Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);
        if (($_POST['remove'] ?? null) > 0) {
            \Database::query("DELETE FROM rozpis_item WHERE ri_id='?'", $id);
        }
        $items = \DBRozpis::getLessons($id);
        //Update all
        foreach ($items as &$item) {
            $item['ri_partner'] = $_POST[$item['ri_id'] . '-partner'];
            $item['ri_od'] = trim($_POST[$item['ri_id'] . '-od']) . ':00';
            $item['ri_do'] = trim($_POST[$item['ri_id'] . '-do']) . ':00';
            $item['ri_lock'] = ($_POST[$item['ri_id'] . '-lock'] ?? '') ? 1 : 0;
        }
        \DBRozpis::editMultipleLessons($items);

        //Try to add a new item
        if ($_POST['add_od'] && $_POST['add_do']) {
            $form = static::checkAdd();
            if (!$form->isValid()) {
                \Message::warning($form->getMessages());
            } else {
                \DBRozpis::addLesson(
                    $id,
                    $_POST['add_partner'],
                    trim($_POST['add_od']) . ':00',
                    trim($_POST['add_do']) . ':00',
                    (int) (bool) $_POST['add_lock']
                );
            }
        }
        \Redirect::to($_SERVER['REQUEST_URI']);
    }

    protected static function checkAdd(): \Form
    {
        $f = new \Form();
        $f->checkNumeric($_POST['add_partner'], 'Neplatný partner u přidávané lekce');
        $f->checkTime($_POST['add_od'], 'Neplatný formát času "od" u přidávané lekce');
        $f->checkTime($_POST['add_do'], 'Neplatný formát času "do" u přidávané lekce');
        return $f;
    }
}
