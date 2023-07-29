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
        $items = \Database::queryArray(
            "SELECT p_id,u_id,u_login,u_jmeno,u_prijmeni,ri_id,ri_id_rodic,ri_partner,ri_od,ri_do,ri_lock
            FROM rozpis_item LEFT JOIN pary ON ri_partner=p_id LEFT JOIN users ON p_id_partner=u_id
            WHERE ri_id_rodic='?'
            ORDER BY ri_od",
            $id,
        );
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
        $items = \Database::queryArray(
            "SELECT p_id,u_id,u_login,u_jmeno,u_prijmeni,ri_id,ri_id_rodic,ri_partner,ri_od,ri_do,ri_lock
            FROM rozpis_item LEFT JOIN pary ON ri_partner=p_id LEFT JOIN users ON p_id_partner=u_id
            WHERE ri_id_rodic='?'
            ORDER BY ri_od",
            $id,
        );
        //Update all
        foreach ($items as &$item) {
            $item['ri_partner'] = $_POST[$item['ri_id'] . '-partner'];
            $item['ri_od'] = trim($_POST[$item['ri_id'] . '-od']) . ':00';
            $item['ri_do'] = trim($_POST[$item['ri_id'] . '-do']) . ':00';
            $item['ri_lock'] = ($_POST[$item['ri_id'] . '-lock'] ?? '') ? 1 : 0;
        }

        $ids = array_map(fn($item) => $item['ri_id'], $items);
        $items = array_map(
            fn($item) => array_intersect_key(
                $item,
                array_flip(['ri_partner', 'ri_od', 'ri_do', 'ri_lock'])
            ),
            $items
        );

        $rows = \Database::escapeArray(array_values($items));

        $columns_string = [];
        foreach (array_keys(reset($items)) as $col_index => $col) {
            $s = $col . ' = CASE';
            foreach ($rows as $row_index => $row) {
                $value = $row[$col_index] ? "'{$row[$col_index]}'" : "'0'" ;
                if ($col == 'ri_partner' && $value == "'0'") {
                    $value = 'NULL';
                }
                $s .= " WHEN ri_id='" . $ids[$row_index] . "' THEN " . $value;
            }
            $s .= ' ELSE ' . $col . ' END';
            $columns_string[] = $s;
        }
        \Database::query('UPDATE rozpis_item SET ' . implode(', ', $columns_string) . " WHERE ri_id IN ('" );

        //Try to add a new item
        if ($_POST['add_od'] && $_POST['add_do']) {
            $form = static::checkAdd();
            if (!$form->isValid()) {
                \Message::warning($form->getMessages());
            } else {
                $user_id = $_POST['add_partner'] ? "'{$_POST['add_partner']}'" : "NULL";
                \Database::query(
                    "INSERT INTO rozpis_item (ri_id_rodic,ri_partner,ri_od,ri_do,ri_lock) VALUES ('?',$user_id,'?','?','?')",
                    $id,
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
