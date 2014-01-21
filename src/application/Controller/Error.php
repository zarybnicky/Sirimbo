<?php
namespace TKOlomouc\Controller;

class Error extends ControllerAbstract
{
    public function checkPermissions()
    {
        ;
    }

    public function view()
    {
        if (!$this->request->get('id')) {
            $this->request->get('id', '');
        }
        $id = str_replace('_', '', $this->request->get('id'));
        
        $view = new \TKOlomouc\View\Main\Error($this->request, $this->request->getView());
        $view->setErrorId($id);
        echo $view->render();
    }

    public function sidebar()
    {
        ;
    }
}
