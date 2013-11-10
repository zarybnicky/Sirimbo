<?php
class Controller_Kontakt extends Controller_Abstract
{
    function sidebar() { }
    function view($id = null) {
        $this->render('src/application/View/Main/Kontakt.inc');
    }
}
?>