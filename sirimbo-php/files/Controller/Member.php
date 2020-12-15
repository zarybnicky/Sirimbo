<?php
namespace Olymp\Controller;

class Member
{
    public static function login()
    {
        if (\Session::isLogged()) {
            $uri = $_GET['return'] ? $_GET['return'] : '/member';
            new \RedirectHelper($uri);
        }
        new \RenderHelper('files/View/Main/Login.inc');
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
            return new \RenderHelper('files/View/Empty.inc', [
                'header' => 'Upozornění',
                'notice' => 'Žádná upozornění nejsou k dispozici'
            ]);
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
                    'addedTimestamp' => formatTimestamp($item['up_timestamp_add']),
                    'text' => stripslashes($item['up_text'])
                ];
            },
            $data
        );

        new \RenderHelper('files/View/Member/Nastenka.inc', [
            'header' => 'Upozornění',
            'data' => $data,
            'navigation' => $pager->getNavigation()
        ]);
    }

    public static function download()
    {
        \Permissions::checkError('dokumenty', P_VIEW);
        if (!$_GET['id']) {
            new \RedirectHelper('/member/dokumenty');
        }

        $data = \DBDokumenty::getSingleDokument($_GET['id']);
        $path = $data['d_path'];
        if (!is_file($path) || !($file = fopen($path, 'rb'))) {
            new \MessageHelper('warning', 'Soubor nebyl nalezen.');
            return new \RedirectHelper('/member/dokumenty');
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
        $kat = $_GET['kat'];
        if (ctype_digit($kat)) {
            $dokumenty = \DBDokumenty::getDokumentyByKategorie($kat);
        } else {
            $dokumenty = \DBDokumenty::getDokumenty();
        }

        new \RenderHelper('files/View/Member/Dokumenty.inc', [
            'header' => 'Dokumenty',
            'kat' => $_GET['kat'] ?: '',
            'data' => array_map(
                fn($item) => [
                    'id' => $item['d_id'],
                    'name' => $item['d_name'],
                    'fileName' => $item['d_filename'],
                    'kategorie' => \Settings::$documentTypes[$item['d_kategorie']],
                    'uploadedBy' => $item['u_jmeno'] . ' ' . $item['u_prijmeni']
                ],
                $dokumenty
            )
        ]);
    }
}
