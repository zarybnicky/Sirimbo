<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Konzole extends Controller_Admin
{
    function __construct() {
        Permissions::checkError('konzole', P_OWNED);
    }
    function view($id = null) {
        if (!empty($_POST) && post('code')) {
            $r = eval(stripslashes(post('code')));
            if ($r === false)
                echo $this->notice('Kód obsahuje syntaktickou chybu');
            elseif (!empty($r))
                echo $this->notice(dump($r));
            else
                echo $this->notice('Success!');
        }
        echo '<form action="' .  $_SERVER['REQUEST_URI'] . '" method="post">';
        echo 'Kód:<br/>';
        echo '<textarea name="code" rows="10" cols="20"></textarea><br/>';
        echo '<button type="submit">Zpracovat</button>';
        echo '</form>';
    }
}
?>