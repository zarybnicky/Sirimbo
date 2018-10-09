<?php
class Controller_Oklubu extends Controller_Abstract
{
    public function view()
    {
        $this->redirect('/oklubu/obecne');
    }

    public function obecne()
    {
        $this->render('files/View/Main/OKlubu/Main.inc', [
            'header' => 'O klubu'
        ]);
    }

    public function historie()
    {
        $this->render('files/View/Main/OKlubu/Historie.inc', [
            'header' => 'Historie klubu'
        ]);
    }

    public function klubovi()
    {
        $this->render('files/View/Main/OKlubu/TreneriInt.inc', [
            'header' => 'Kluboví trenéři'
        ]);
    }

    public function externi()
    {
        $this->render('files/View/Main/OKlubu/TreneriExt.inc', [
            'header' => 'Externí trenéři'
        ]);
    }

    public function saly()
    {
        $this->render('files/View/Main/OKlubu/Saly.inc', [
            'header' => 'Kde trénujeme'
        ]);
    }

    public function navbar()
    {
        return array_merge(parent::navbar(), [include SETTINGS . '/menu/oklubu.php']);
    }
}
