<?php
class Controller_Admin extends Controller_Abstract
{
    public function __construct()
    {
        Permissions::checkError('nastenka', P_OWNED);
    }

    public function view($request)
    {
        $this->render('files/View/Admin/Home.inc', [
            'header' => 'Administrace'
        ]);
    }
}