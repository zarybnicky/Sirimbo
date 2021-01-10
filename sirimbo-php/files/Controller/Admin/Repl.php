<?php
namespace Olymp\Controller\Admin;

class Repl
{
    public static function get()
    {
        \Permissions::checkError('konzole', P_OWNED);
        echo '<form method="post">Kód:<br/>'
            . '<textarea name=code rows=10 cols=10>'
            . ($_POST['code'] ?? '')
            . '</textarea>',
            '<button class="btn btn-primary">Zpracovat</button>';
    }

    public static function post()
    {
        \Permissions::checkError('konzole', P_OWNED);
        if ($_POST['code']) {
            $r = eval(stripslashes($_POST['code']));
            if ($r === false) {
                echo 'Kód obsahuje syntaktickou chybu';
            } elseif (!empty($r)) {
                echo '<pre>' . print_r($r, true) . '</pre>';
            } else {
                echo 'Success!';
            }
        }
        static::get();
    }
}
