<?php
class Controller_Registrace extends Controller_Abstract
{
    public function view($request)
    {
        if (!$request->post()) {
            $this->render(
                'files/View/Main/Registrace.inc',
                array(
                    'username' => '',
                    'pass' => '',
                    'jmeno' => '',
                    'prijmeni' => '',
                    'pohlavi' => '',
                    'email' => '',
                    'telefon' => '',
                    'narozeni' => '',
                    'poznamky' => '',
                    'other' => ''
                )
            );
            return;
        }

        $narozeni = (string) $this->date('narozeni')->getPost($request);
        $poznamkyMap = array(
            'parent' => 'Rodič tanečníka',
            'dancer' => 'Tanečník/tanečníce',
            'other' => $request->post('other')
        );
        $poznamky = $poznamkyMap[$request->post('poznamky')];

        $f = new Form();
        $f->checkLogin($request->post('username'), 'Špatný formát přihlašovacího jména', 'username');
        $f->checkPassword($request->post('pass'), 'Špatný formát hesla', 'pass');
        $f->checkLength($request->post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
        $f->checkLength($request->post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
        $f->checkInArray($request->post('pohlavi'), array('m', 'f'), 'Neplatné pohlaví', 'pohlavi');
        $f->checkEmail($request->post('email'), 'Neplatný formát emailu', 'email');
        $f->checkPhone($request->post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');
        $f->checkDate($narozeni, 'Neplatné datum narození', 'narozeni');

        if (!$f->isValid()) {
            $this->redirect()->setMessage(implode('<br/>', $f->getMessages()));
            $this->render(
                'files/View/Main/Registrace.inc',
                array(
                    'username' => $request->post('username') ?: '',
                    'pass' => $request->post('pass') ?: '',
                    'jmeno' => $request->post('jmeno') ?: '',
                    'prijmeni' => $request->post('prijmeni') ?: '',
                    'pohlavi' => $request->post('pohlavi') ?: '',
                    'email' => $request->post('email') ?: '',
                    'telefon' => $request->post('telefon') ?: '',
                    'narozeni' => $request->post('narozeni') ?: '',
                    'poznamky' => $request->post('poznamky') ?: '',
                    'other' => $request->post('other') ?: ''
                )
            );
            return;
        }

        User::register(
            strtolower($request->post('username')),
            $request->post('pass'),
            $request->post('jmeno'),
            $request->post('prijmeni'),
            $request->post('pohlavi'),
            $request->post('email'),
            $request->post('telefon'),
            $narozeni,
            $poznamky
        );
        $this->redirect(
            '/home',
            'Registrace úspěšně proběhla.<br /><br />'
            . 'Během několika dnů vám na email příjde potvrzení vašeho účtu, '
            . 'které vyřizuje administrátor ručně.'
        );
    }
}
