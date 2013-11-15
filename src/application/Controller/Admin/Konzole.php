<?php
namespace TKOlomouc\Controller\Admin;

use TKOlomouc\Controller\Admin;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\Debug;

class Konzole extends Admin
{
    function __construct() {
        Permissions::checkError('konzole', P_OWNED);
    }
    function view($id = null) {
        if (!empty($_POST) && post('code')) {
            $r = eval(stripslashes(post('code')));
            if ($r === false)
                notice('Kód obsahuje syntaktickou chybu');
            elseif (!empty($r))
                notice(dump($r));
            else
                notice('Success!');
        }
        echo '<form action="' .  $_SERVER['REQUEST_URI'] . '" method="post">';
        echo 'Kód:<br/>';
        echo '<textarea name="code" rows="10" cols="20"></textarea><br/>';
        echo '<button type="submit">Zpracovat</button>';
        echo '</form>';
    }
}
?>