<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Konzole extends Controller_Admin
{
    public function __construct()
    {
        Permissions::checkError('konzole', P_OWNED);
    }
    public function view($request)
    {
        if ($request->post('code')) {
            $r = eval(stripslashes($request->post('code')));
            if ($r === false) {
                echo $this->notice('Kód obsahuje syntaktickou chybu');
            } elseif (!empty($r)) {
                echo $this->notice(dump($r));
            } else {
                echo $this->notice('Success!');
            }
        }
        echo '<form action="' .  $request->server('REQUEST_URI') . '" method="post">';
        echo 'Kód:<br/>';
        echo '<textarea name="code" rows="10" cols="20"></textarea><br/>';
        echo '<button type="submit">Zpracovat</button>';
        echo '</form>';
    }
}
