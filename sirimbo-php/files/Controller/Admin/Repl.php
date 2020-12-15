<?php
namespace Olymp\Controller\Admin;

class Repl
{
    public static function get()
    {
        \Permissions::checkError('konzole', P_OWNED);
        echo new \Tag(
            'form',
            ['action' => '', 'method' => 'post'],
            'Kód:<br/>',
            new \Tag(
                'textarea',
                ['name' => 'code', 'rows' => 10, 'cols' => 10],
                $_POST['code'] ?: ''
            ),
            new \SubmitHelper('Zpracovat'),
        );
    }

    public static function post()
    {
        \Permissions::checkError('konzole', P_OWNED);
        if ($_POST['code']) {
            $r = eval(stripslashes($_POST['code']));
            if ($r === false) {
                echo new \NoticeHelper('Kód obsahuje syntaktickou chybu');
            } elseif (!empty($r)) {
                echo new \NoticeHelper('<pre>' . var_export($r, true) . '</pre>');
            } else {
                echo new \NoticeHelper('Success!');
            }
        }
        static::get();
    }
}
