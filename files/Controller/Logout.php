<?php
class Controller_Logout implements Controller_Interface {
    function view($id = null) {
        User::logout();
        echo "Byli jste úspěšně odhlášeni.";
    }
}
?>