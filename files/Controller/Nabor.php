<?php
class Controller_Nabor extends Controller_Abstract
{
    function view($id = null) {
        $this->render('files/Main/Nabor/Main.inc', array(), true);
    }
}
