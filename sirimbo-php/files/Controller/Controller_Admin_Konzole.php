<?php
class Controller_Admin_Konzole extends Controller_Abstract
{
    public function __construct()
    {
        Permissions::checkError('konzole', P_OWNED);
    }

    public function view($request)
    {
        if ($request->post('code')) {
            $r = eval(stripslashes($request->post('code')));
            if ($r === false) {
                echo $this->notice('Kód obsahuje syntaktickou chybu');
            } elseif (!empty($r)) {
                echo $this->notice(dump($r));
            } else {
                echo $this->notice('Success!');
            }
        }
        echo (string) new Tag(
            'form',
            ['action' => '', 'method' => 'post'],
            'Kód:<br/>',
            new Tag(
                'textarea',
                ['name' => 'code', 'rows' => 10, 'cols' => 10],
                $request->post('code') ?: ''
            ),
            $this->submit('Zpracovat')->render()
        );
    }
}
