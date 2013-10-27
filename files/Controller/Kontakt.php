<?php
class Controller_Kontakt extends Controller_Abstract
{
    function sidebar() { }
    function view($id = null) {
        $this->render('files/View/Main/Kontakt.inc');
    }
}
?>