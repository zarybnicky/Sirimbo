<?php
class Controller_Oklubu extends Controller_Abstract
{
    public function view($request)
    {
        $this->redirect('/oklubu/obecne');
    }

    public function obecne($request)
    {
        $this->render(
            'files/View/Main/OKlubu/Main.inc',
            array(
                'sidebar' => $this->sidebar($request->getUri())
            )
        );
    }

    public function historie($request)
    {
        $this->render(
            'files/View/Main/OKlubu/Historie.inc',
            array(
                'sidebar' => $this->sidebar($request->getUri())
            )
        );
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

    public function saly($request)
    {
        $this->render(
            'files/View/Main/OKlubu/Saly.inc',
            array(
                'sidebar' => $this->sidebar($request->getUri())
            )
        );
    }

    public function navbar()
    {
        return parent::navbar() .
            new Navbar(
                include SETTINGS . '/menu/oklubu.php',
                false
            );
    }

    public function sidebar($uri)
    {
        return new Sidebar(
            include SETTINGS . '/menu/oklubu.php',
            $uri
        );
    }
}
