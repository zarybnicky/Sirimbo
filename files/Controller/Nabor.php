<?php
class Controller_Nabor extends Controller_Abstract
{
    public function view($id = null) {
        $this->render('files/Main/Nabor/Main.inc', array(), true);
    }
}
