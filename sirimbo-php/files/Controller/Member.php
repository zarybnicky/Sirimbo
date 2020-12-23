<?php
namespace Olymp\Controller;

class Member
{
    public static function login()
    {
        if (\Session::getUser()) {
            \Redirect::to($_GET['return'] ?? '/member');
        }
        \Render::page('files/View/Main/Login.inc');
    }

    public static function get()
    {
        \Permissions::checkError('nastenka', P_VIEW);

        $pager = new \Paging(new \DBNastenka());
        $pager->setCurrentPage($_GET['p'] ?? null);
        $pager->setItemsPerPage($_GET['c'] ?? null);
        $pager->setDefaultItemsPerPage(10);
        $data = $pager->getItems();

        if (empty($data)) {
            \Render::page('files/View/Empty.inc', [
                'header' => 'Upozornění',
                'notice' => 'Žádná upozornění nejsou k dispozici'
            ]);
            return;
        }

        $data = array_map(
            function ($item) {
                $skupiny = array_map(
                    fn($skupina) => new \ColorboxHelper($skupina['ups_color'], $skupina['ups_popis']),
                    \DBNastenka::getNastenkaSkupiny($item['up_id'])
                );
                return [
                    'id' => $item['up_id'],
                    'nadpis' => $item['up_nadpis'],
                    'canEdit' => \Permissions::check('nastenka', P_OWNED, $item['up_kdo']),
                    'skupinyBoxes' => implode('', $skupiny),
                    'addedBy' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'addedTimestamp' => \Format::timestamp($item['up_timestamp_add']),
                    'text' => stripslashes($item['up_text'])
                ];
            },
            $data
        );

        \Render::page('files/View/Member/Nastenka.inc', [
            'header' => 'Upozornění',
            'data' => $data,
            'navigation' => $pager->getNavigation()
        ]);
    }

    public static function download()
    {
        \Permissions::checkError('dokumenty', P_VIEW);
        if (!$_GET['id']) {
            \Redirect::to('/member/dokumenty');
        }

        $data = \DBDokumenty::getSingleDokument($_GET['id']);
        $path = $data['d_path'];
        if (!is_file($path) || !($file = fopen($path, 'rb'))) {
            \Message::warning('Soubor nebyl nalezen.');
            \Redirect::to('/member/dokumenty');
        }

        header('Pragma: no-cache');
        header('Content-Type: application/octet-stream');
        header('Content-Disposition: inline; filename="' . $data['d_filename'] . '"');
        fpassthru($file);
        fclose($file);
    }

    public static function dokumenty()
    {
        \Permissions::checkError('dokumenty', P_VIEW);
        $kat = $_GET['kat'] ?? '';
        \Render::page('files/View/Member/Dokumenty.inc', [
            'header' => 'Dokumenty',
            'kat' => $kat,
            'data' => array_map(
                fn($item) => [
                    'id' => $item['d_id'],
                    'name' => $item['d_name'],
                    'fileName' => $item['d_filename'],
                    'kategorie' => Admin\Dokumenty::$types[$item['d_kategorie']],
                    'uploadedBy' => "{$item['u_jmeno']} {$item['u_prijmeni']}",
                ],
                ctype_digit($kat)
                ? \DBDokumenty::getDokumentyByKategorie($kat)
                : \DBDokumenty::getDokumenty()
            )
        ]);
    }
}
