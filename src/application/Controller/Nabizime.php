<?php
namespace TKOlomouc\Controller;

use TKOlomouc\View\Helper\Sidebar;
use TKOlomouc\Utility\Request;

class Nabizime extends ControllerAbstract
{
	protected function checkPermissions()
	{
        ;
	}

    public function view()
    {
        $this->redirect('/nabizime/obecne');
    }

    protected function show($file)
    {
        $view = new \TKOlomouc\View\StaticPage($this->request, $this->request->getView());
        $view->setFile($file);
        $view->set('sidemenu', $this->sidebar());
        echo $view->render();
    }

    public function obecne()
    {
        $this->show('Main/Nabizime/Main');
    }

    public function vystoupeni()
    {
        $this->show('Main/Nabizime/Vystoupeni');
    }

    public function individualky()
    {
        $this->show('Main/Nabizime/Individualky');
    }

    public function seminare()
    {
        $this->show('Main/Nabizime/Seminare');
    }

    public function soustredeni()
    {
        $this->show('Main/Nabizime/Seminare');
    }

    public function sidebar()
    {
        $s = new Sidebar($this->request);

        $out = $s->menuHeader();
        $out .= $s->menuItem('Nabízíme', '/nabizime/obecne');
        $out .= $s->menuItem('Taneční vystoupení', '/nabizime/vystoupeni');
        $out .= $s->menuItem('Individuální lekce', '/nabizime/individualky');
        $out .= $s->menuItem('Skupinové semináře', '/nabizime/seminare');
        $out .= $s->menuItem('Taneční soustředění', '/nabizime/soustredeni');

        $out .= $s->commonItems();

        return $out;
    }
}
