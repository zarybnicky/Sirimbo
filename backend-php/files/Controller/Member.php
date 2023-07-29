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

    public static function loginPost()
    {
        if (\Session::getUser()) {
            \Redirect::to($_GET['return'] ?? '/member');
        }
        \Redirect::to('/');
    }

    public static function logout()
    {
        session_destroy();
        \Redirect::to('/');
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
                ? \Database::queryArray(
                    "SELECT * FROM dokumenty LEFT JOIN users ON d_kdo=u_id WHERE d_kategorie='?' ORDER BY d_id DESC",
                    $kat
                )
                : \Database::queryArray(
                    "SELECT * FROM dokumenty LEFT JOIN users ON d_kdo=u_id ORDER BY d_id DESC"
                )
            )
        ]);
    }
}
