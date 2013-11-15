<?php
namespace TKOlomouc\Controller;

class Kontakt extends ControllerAbstract
{
    function sidebar() { }
    function view($id = null) {
        $this->render('src/application/View/Main/Kontakt.inc');
    }
}
?>