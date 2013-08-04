<?php
class Controller_Logout extends Controller_Abstract {
    function view($id = null) {
        User::logout();
        echo "Byli jste úspěšně odhlášeni.";
    }
    function sidebar() {
    	
    }
}
?>