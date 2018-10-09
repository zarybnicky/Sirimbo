<?php
class Controller_Nabizime extends Controller_Abstract
{
    public function view()
    {
        $this->redirect('/nabizime/obecne');
    }

    public function obecne()
    {
        $this->render('files/View/Main/Nabizime/Main.inc', [
            'header' => 'Nabízíme'
        ]);
    }

    public function vystoupeni()
    {
        $this->render('files/View/Main/Nabizime/Vystoupeni.inc', [
            'header' => 'Taneční vystoupení'
        ]);
    }

    public function individualky()
    {
        $this->render('files/View/Main/Nabizime/Individualky.inc', [
            'header' => 'Individuální lekce'
        ]);
    }

    public function seminare()
    {
        $this->render('files/View/Main/Nabizime/Seminare.inc');
    }

    public function soustredeni()
    {
        $this->render('files/View/Main/Nabizime/Seminare.inc');
    }

    public function navbar()
    {
        return array_merge(parent::navbar(), [include SETTINGS . '/menu/nabizime.php']);
    }
}
