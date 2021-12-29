<?php
namespace Olymp\Controller;

class Member
{
    public static function get()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        \Render::twig('Member/Nastenka.twig');
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
        \Render::twig('Member/Dokumenty.twig', [
            'kat' => $kat,
            'categories' => ['' => '--- vše ---'] + Admin\Dokumenty::$types,
            'data' => array_map(
                fn($item) => [
                    'id' => $item['d_id'],
                    'name' => $item['d_name'],
                    'fileName' => $item['d_filename'],
                    'kategorie' => Admin\Dokumenty::$types[$item['d_kategorie']],
                    'uploadedBy' => "{$item['u_jmeno']}\u{00A0}{$item['u_prijmeni']}",
                ],
                ctype_digit($kat)
                ? \DBDokumenty::getDokumentyByKategorie($kat)
                : \DBDokumenty::getDokumenty()
            )
        ]);
    }
}
