<?php
require_once 'files/Controller/Oklubu/Treneri.php';
class Controller_Oklubu_Treneri_Externi extends Controller_Oklubu_Treneri
{
    public function view($request)
    {
        $this->render(
            'files/View/Main/OKlubu/TreneriExt.inc',
            ['sidebar' => $this->sidebar($request->getUri())]
        );
    }
}
