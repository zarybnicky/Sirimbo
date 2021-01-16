<?php
namespace Olymp\Controller;

class Member
{
    public static function login()
    {
        if (\Session::getUser()) {
            \Redirect::to($_GET['return'] ?? '/member');
        }
        \Render::twig('Main/Login.twig');
    }

    public static function get()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        $pager = new \Paging(new \DBNastenka());
        $pager->setCurrentPage($_GET['p'] ?? null);
        $pager->setItemsPerPage($_GET['c'] ?? null);
        $pager->setDefaultItemsPerPage(10);
        \Render::twig('Member/Nastenka.twig', [
            'navigation' => $pager->getNavigation(),
            'data' => array_for($pager->getItems(), fn($item) => [
                'id' => $item['up_id'],
                'nadpis' => $item['up_nadpis'],
                'canEdit' => \Permissions::check('nastenka', P_OWNED, $item['up_kdo']),
                'skupinyBoxes' => \DBNastenka::getNastenkaSkupiny($item['up_id']),
                'addedBy' => "{$item['u_jmeno']} {$item['u_prijmeni']}",
                'addedTimestamp' => $item['up_timestamp_add'],
                'text' => stripslashes($item['up_text'])
            ]),
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
        $categories = ['' => '--- vše ---'] + array_map(
            fn($x) => str_replace(' ', "\u{00A0}", $x),
            Admin\Dokumenty::$types
        );
        \Render::twig('Member/Dokumenty.twig', [
            'kat' => $kat,
            'categories' => $categories,
            'data' => array_map(
                fn($item) => [
                    'id' => $item['d_id'],
                    'name' => $item['d_name'],
                    'fileName' => $item['d_filename'],
                    'kategorie' => $categories[$item['d_kategorie']],
                    'uploadedBy' => "{$item['u_jmeno']}\u{00A0}{$item['u_prijmeni']}",
                ],
                ctype_digit($kat)
                ? \DBDokumenty::getDokumentyByKategorie($kat)
                : \DBDokumenty::getDokumenty()
            )
        ]);
    }
}
