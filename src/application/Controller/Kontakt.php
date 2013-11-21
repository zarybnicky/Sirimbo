<?php
namespace TKOlomouc\Controller;

class Kontakt extends ControllerAbstract
{
    public function view($id = null)
    {
        $this->render('src/application/View/Main/Kontakt.inc');
    }

    public function sidebar()
    {
        ;
    }
}
