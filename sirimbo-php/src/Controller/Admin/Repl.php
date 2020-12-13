<?php
namespace Olymp\Controller\Admin;

use \Permissions;
use \Tag;

class Repl
{
    use \HelperTrait;

    public static function get()
    {
        Permissions::checkError('konzole', P_OWNED);
        echo new Tag(
            'form',
            ['action' => '', 'method' => 'post'],
            'Kód:<br/>',
            new Tag(
                'textarea',
                ['name' => 'code', 'rows' => 10, 'cols' => 10],
                $_POST['code'] ?: ''
            ),
            $this->submit('Zpracovat')->render()
        );
    }

    public static function post()
    {
        Permissions::checkError('konzole', P_OWNED);
        if ($_POST['code']) {
            $r = eval(stripslashes($_POST['code']));
            if ($r === false) {
                echo $this->notice('Kód obsahuje syntaktickou chybu');
            } elseif (!empty($r)) {
                echo $this->notice(dump($r));
            } else {
                echo $this->notice('Success!');
            }
        }
        static::get();
    }
}
