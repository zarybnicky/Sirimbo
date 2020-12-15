<?php
namespace Olymp\Controller\Admin;

class Akce
{
    public static function detailGet($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        if (!$akce = \DBAkce::getSingleAkce($id)) {
            new \MessageHelper('warning', 'Akce s takovým ID neexistuje');
            new \RedirectHelper('/admin/akce');
        }
        $data = [
            'id' => $akce['a_id'],
            'jmeno' => $akce['a_jmeno'],
            'kde' => $akce['a_kde'],
            'datum' => formatDate($akce['a_od'])
                . (($akce['a_od'] != $akce['a_do'])
                ? ' - ' . formatDate($akce['a_do'])
                : ''),
            'kapacita' => $akce['a_kapacita'],
            'volno' => $akce['a_kapacita'] - count(\DBAkce::getAkceItems($id)),
            'showForm' => \Permissions::check('akce', P_MEMBER)
                && !$akce['a_lock'],
            'canEdit' => \Permissions::check('akce', P_OWNED),
            'info' => nl2br($akce['a_info'])
        ];

        $userSelect = new \UserSelectHelper(\DBUser::getActiveUsers());
        $items = array_map(
            fn($item) => [
                'name' => (string) $userSelect->name($item['ai_id'] . '-user')->set($item['ai_user']),
                'removeButton' => (new \SubmitHelper('Odstranit'))->data('remove', $item['ai_id'])
            ],
            \DBAkce::getAkceItems($id)
        );
        $items[] = [
            'name' => (string) $userSelect->name('add-user')->set(0),
            'removeButton' => (new \SubmitHelper('Přidat'))->data('add', 'add')
        ];

        new \RenderHelper('files/View/Admin/Akce/Detail.inc', [
            'header' => 'Správa akcí',
            'data' => $data,
            'items' => $items
        ]);
    }

    public static function detailPost($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        if (!\DBAkce::getSingleAkce($id)) {
            new \MessageHelper('warning', 'Akce s takovým ID neexistuje');
            new \RedirectHelper('/admin/akce');
        }

        if (isset($_POST["remove"])) {
            \DBAkce::removeAkceItem($_POST["remove"]);
        }

        foreach (\DBAkce::getAkceItems($id) as $item) {
            $user = $_POST[$item["ai_id"] . '-user'];

            if (!$user) {
                \DBAkce::removeAkceItem($item['ai_id']);
            } elseif ($user != $item["ai_user"]) {
                $data = \DBUser::getUserData($user);
                list($year) = explode('-', $data['u_narozeni']);
                \DBAkce::editAkceItem($item["ai_id"], $user, $year);
            }
        }

        if (is_numeric($_POST["add-user"]) && $_POST['add-user'] > 0) {
            $user = $_POST["add-user"];
            $data = \DBUser::getUserData($user);
            list($year) = explode('-', $data['u_narozeni']);

            \DBAkce::addAkceItem($id, $user, $year);
            $_POST['add-user'] = 0;
        }
        new \RedirectHelper('/admin/akce/detail/' . $id);
    }

    public static function dokumentyGet($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        if (!($akce = \DBAkce::getSingleAkce($id))) {
            new \MessageHelper('warning', 'Akce s takovým ID neexistuje');
            new \RedirectHelper('/admin/akce');
        }
        $documents = array_filter(explode(',', $akce["a_dokumenty"]));

        $akce = [
            'id' => $akce['a_id'],
            'jmeno' => $akce['a_jmeno'],
            'kde' => $akce['a_kde'],
            'datum' => formatDate($akce['a_od'])
            . (($akce['a_od'] != $akce['a_do'])
               ? ' - ' . formatDate($akce['a_do']) : ''),
            'kapacita' => $akce['a_kapacita'],
            'volno' => $akce['a_kapacita'] - count(\DBAkce::getAkceItems($id)),
            'showForm' => \Permissions::check('akce', P_MEMBER) && !$akce['a_lock'],
            'canEdit' => \Permissions::check('akce', P_OWNED)
        ];

        $documents = array_map(
            fn($item) => [
                'name' => $item['d_name'],
                'category' => \Settings::$documentTypes[$item['d_kategorie']],
                'removeButton' => (new \SubmitHelper('Odstranit'))->data('remove', $item['d_id'])
            ],
            \DBDokumenty::getMultipleById($documents)
        );

        $allDocuments = [];
        foreach ([2, 3, 0] as $category) {
            foreach (\DBDokumenty::getDokumentyByKategorie($category) as $item) {
                $allDocuments[$item['d_id']] =
                    \Settings::$documentTypes[$item['d_kategorie']] . ' - ' .
                    $item['d_name'];
            }
        }
        $documentSelect = new \SelectHelper('add-id', array_merge(['' => '---'], $allDocuments));
        $documents[] = [
            'name' => (string) $documentSelect,
            'category' => (new \SubmitHelper('Přidat'))->data('add', 'add'),
            'removeButton' => ''
        ];
        new \RenderHelper('files/View/Admin/Akce/Dokumenty.inc', [
            'header' => 'Správa akcí',
            'data' => $akce,
            'documents' => $documents
        ]);
    }

    public static function dokumentyPost($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        if (!($akce = \DBAkce::getSingleAkce($id))) {
            new \MessageHelper('warning', 'Akce s takovým ID neexistuje');
            new \RedirectHelper('/admin/akce');
        }
        $documents = array_filter(explode(',', $akce["a_dokumenty"]));

        $changed = false;
        if (isset($_POST["remove"])) {
            unset($documents[array_search($_POST['remove'], $documents)]);
            $documents = array_values($documents);
            $changed = true;
        }
        if (isset($_POST["add-id"]) && \DBDokumenty::getSingleDokument($_POST["add-id"])) {
            $documents[] = $_POST["add-id"];
            unset($_POST['add-id']);
            $changed = true;
        }
        if ($changed) {
            \DBAkce::editAkce(
                $akce["a_id"],
                $akce["a_jmeno"],
                $akce["a_kde"],
                $akce["a_info"],
                $akce["a_od"],
                $akce["a_do"],
                $akce["a_kapacita"],
                implode(',', $documents),
                $akce["a_lock"],
                $akce['a_visible'],
            );
        }
        new \RedirectHelper('/admin/akce/dokumenty/' . $id);
    }
}
