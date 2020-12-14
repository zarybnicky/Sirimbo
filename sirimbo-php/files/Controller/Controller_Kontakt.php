<?php
class Controller_Kontakt
{
    public function view($request)
    {
        new \RenderHelper('files/View/Main/Kontakt.inc', [
            'header' => 'Kontakt'
        ]);
    }
}
