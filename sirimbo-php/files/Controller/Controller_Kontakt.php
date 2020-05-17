<?php
class Controller_Kontakt extends Controller_Abstract
{
    public function view($request)
    {
        $this->render('files/View/Main/Kontakt.inc', [
            'header' => 'Kontakt'
        ]);
    }
}
