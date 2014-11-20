<?php
require_once 'files/Controller/Oklubu.php';
class Controller_Oklubu_Treneri extends Controller_Oklubu
{
    public function view($id = null) {
        $this->redirect('/oklubu/obecne');
    }

    public function klubovi($id = null) {
        $this->render('files/View/Main/OKlubu/TreneriInt.inc');
    }

    public function externi($id = null) {
        $this->render('files/View/Main/OKlubu/TreneriExt.inc');
    }

    public function sidebar() {
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
            )
        );
    }
}