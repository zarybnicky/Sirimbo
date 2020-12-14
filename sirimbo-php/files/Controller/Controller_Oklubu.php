<?php
class Controller_Oklubu extends Controller_Abstract
{
    public function view($request)
    {
        new \RedirectHelper('/');
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

    public function saly($request)
    {
        new \RenderHelper('files/View/Main/OKlubu/Saly.inc', [
            'header' => 'Kde trénujeme'
        ]);
    }
}
