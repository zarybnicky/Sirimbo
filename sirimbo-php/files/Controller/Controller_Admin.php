<?php
class Controller_Admin extends Controller_Abstract
{
    public function view($request)
    {
        Permissions::checkError('nastenka', P_OWNED);
        new \RenderHelper('files/View/Admin/Home.inc', [
            'header' => 'Administrace'
        ]);
    }
}
