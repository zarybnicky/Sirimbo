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
            \Utils::submit('Zpracovat');
    }

    public static function post()
    {
        \Permissions::checkError('konzole', P_OWNED);
        if ($_POST['code']) {
            $r = eval(stripslashes($_POST['code']));
            if ($r === false) {
                echo \Utils::notice('Kód obsahuje syntaktickou chybu');
            } elseif (!empty($r)) {
                echo \Utils::notice('<pre>' . var_export($r, true) . '</pre>');
            } else {
                echo \Utils::notice('Success!');
            }
        }
        static::get();
    }
}
