<?php
require_once 'files/Controller/Oklubu.php';
class Controller_Oklubu_Treneri extends Controller_Oklubu
{
    public function view($request)
    {
        $this->redirect('/oklubu/obecne');
    }

    public function klubovi($request)
    {
        $this->render(
            'files/View/Main/OKlubu/TreneriInt.inc',
            array(
                'sidebar' => $this->sidebar($request->getUri())
            )
        );
    }

    public function externi($request)
    {
        $this->render(
            'files/View/Main/OKlubu/TreneriExt.inc',
            array(
                'sidebar' => $this->sidebar($request->getUri())
            )
        );
    }

    public function sidebar($uri)
    {
        return new Sidebar(
            array(
                array(
                    'Kluboví trenéři',
                    '/oklubu/treneri/klubovi',
                    array()
                ),
                array(
                    'Externí trenéři',
                    '/oklubu/treneri/externi',
                    array()
                )
            ),
            $uri
        );
    }
}
