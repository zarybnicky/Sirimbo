<?php
class Controller_Registrace extends Controller_Abstract
{
    public function view($request)
    {
        if (!$request->post()) {
            $skupiny = array_map(
                function ($item) {
                    return [
                        'id' => $item['s_id'],
                        'color' => $item['s_color_rgb'],
                        'popis' => $item['s_name']
                    ];
                },
                DBSkupiny::get()
            );
            $this->render('files/View/Main/Registrace.inc', [
                'header' => 'Registrace',
                'skupiny' => $skupiny,
                'username' => '',
                'pass' => '',
                'jmeno' => '',
                'prijmeni' => '',
                'pohlavi' => '',
                'email' => '',
                'telefon' => '',
                'narozeni' => '',
                'poznamky' => '',
                'dancerName' => '',
                'skupina' => '0',
                'other' => ''
            ]);
            return;
        }

        $narozeni = (string) $this->date('narozeni')->getPost($request);
        $poznamkyMap = [
            'parent' => 'Rodič tanečníka: ' . $request->post('dancer-name'),
            'dancer' => 'Tanečník/tanečnice',
            'other' => 'Jiný vztah: ' . $request->post('other')
        ];
        $poznamky = $poznamkyMap[$request->post('poznamky')];

        $f = new Form();
        $f->checkLogin($request->post('username'), 'Špatný formát přihlašovacího jména', 'username');
        $f->checkPassword($request->post('pass'), 'Špatný formát hesla', 'pass');
        $f->checkInArray($request->post('pohlavi'), ['m', 'f'], 'Neplatné pohlaví', 'pohlavi');
        $f->checkEmail($request->post('email'), 'Neplatný formát emailu', 'email');
        $f->checkPhone($request->post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');
        $f->checkDate($narozeni, 'Neplatné datum narození', 'narozeni');

        if (!$f->isValid()) {
            $skupiny = array_map(
                function ($item) {
                    return [
                        'id' => $item['s_id'],
                        'color' => $item['s_color_rgb'],
                        'popis' => $item['s_name']
                    ];
                },
                DBSkupiny::get()
            );

            $this->redirect()->warning($f->getMessages());
            $this->render('files/View/Main/Registrace.inc', [
                'header' => 'Registrace',
                'skupiny' => $skupiny,
                'username' => $request->post('username') ?: '',
                'pass' => $request->post('pass') ?: '',
                'jmeno' => $request->post('jmeno') ?: '',
                'prijmeni' => $request->post('prijmeni') ?: '',
                'pohlavi' => $request->post('pohlavi') ?: '',
                'email' => $request->post('email') ?: '',
                'telefon' => $request->post('telefon') ?: '',
                'narozeni' => $request->post('narozeni') ?: '',
                'poznamky' => $request->post('poznamky') ?: '',
                'dancerName' => $request->post('dancer-name') ?: '',
                'skupina' => $request->post('skupina') ?: '',
                'other' => $request->post('other') ?: ''
            ]);
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
            $poznamky,
            $request->post('skupina')
        );
        $this->redirect()->success(
            '<h4 class="alert-heading">Registrace úspěšně proběhla.</h4>' .
            '<p>Během několika dnů vám na email příjde potvrzení vašeho účtu, ' .
            'které vyřizuje administrátor ručně.<p>'
        );
        $this->redirect('/');
    }
}
