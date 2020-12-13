<?php
class Controller_Kontakt extends Controller_Abstract
{
    public function view($request)
    {
        new \RenderHelper('files/View/Main/Kontakt.inc', [
            'header' => 'Kontakt'
        ]);
    }
}
