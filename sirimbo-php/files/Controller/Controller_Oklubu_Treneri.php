<?php
class Controller_Oklubu_Treneri
{
    public function view($request)
    {
        new \RedirectHelper('/oklubu/klubovi');
    }

    public function klubovi($request)
    {
        new \RenderHelper('files/View/Main/OKlubu/TreneriInt.inc', [
            'header' => 'Kluboví trenéři'
        ]);
    }

    public function externi($request)
    {
        new \RenderHelper('files/View/Main/OKlubu/TreneriExt.inc', [
            'header' => 'Externí trenéři'
        ]);
    }
}
