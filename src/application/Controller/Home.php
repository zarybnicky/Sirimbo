<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Utility\Request;
use TKOlomouc\Utility\Response;
use TKOlomouc\Model\DBAktuality;

class Home extends ControllerAbstract
{
    public function view()
    {
        if (NABOR && $this->request->getURL() == '/') {
            Response::redirect('/nabor');
            return;
        }
        if ($this->request->getURL() == '/') {
            Response::redirect('/home');
            return;
        }

        $view = new \TKOlomouc\View\Main\Home($this->request, $this->request->getView());

        $view->setClanky(DBAktuality::getAktuality(AKTUALITY_CLANKY));
        $view->setZpravy(DBAktuality::getAktuality(AKTUALITY_KRATKE));
        $view->set('sidebar', $this->sidebar());

        echo $view->render();
    }

    public function checkPermissions()
    {
        ;
    }

    public function sidebar()
    {
        ;
    }
}
