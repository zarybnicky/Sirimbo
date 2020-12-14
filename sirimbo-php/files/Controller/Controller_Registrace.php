<?php
class Controller_Registrace
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
            return new \RenderHelper('files/View/Main/Registrace.inc', [
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
                'street' => '',
                'popisne' => '',
                'orientacni' => '',
                'district' => '',
                'city' => '',
                'postal' => '',
                'nationality' => '203',
                'dancerName' => '',
                'skupina' => '0',
                'other' => ''
            ]);
        }

        $narozeni = (string) new Date($_POST['narozeni'] ?? null);
        $poznamkyMap = [
            'parent' => 'Rodič tanečníka: ' . $request->post('dancer-name'),
            'dancer' => 'Tanečník/tanečnice',
            'other' => 'Jiný vztah: ' . $request->post('other')
        ];
        $poznamky = $poznamkyMap[$request->post('poznamky')];

        $f = new Form();
        $f->checkLogin($request->post('username'), 'Špatný formát přihlašovacího jména', 'username');
        $f->checkPassword($request->post('pass'), 'Špatný formát hesla', 'pass');
        $f->checkEmail($request->post('email'), 'Neplatný formát emailu', 'email');
        $f->checkPhone($request->post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');
        $f->checkDate($narozeni, 'Neplatné datum narození', 'narozeni');
        $f->checkNotEmpty($request->post('orientacni'), 'Vyplňte číslo orientační bydliště', 'orientacni');
        $f->checkNotEmpty($request->post('city'), 'Vyplňte město bydliště', 'city');
        $f->checkNotEmpty($request->post('postal'), 'Vyplňte PSČ bydliště', 'postal');
        $f->checkNotEmpty($request->post('nationality'), 'Vyplňte vaši národnost', 'nationality');

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

            new \MessageHelper('warning', $f->getMessages());
            return new \RenderHelper('files/View/Main/Registrace.inc', [
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
                'street' => $request->post('street') ?: '',
                'popisne' => $request->post('popisne') ?: '',
                'orientacni' => $request->post('orientacni') ?: '',
                'district' => $request->post('district') ?: '',
                'city' => $request->post('city') ?: '',
                'postal' => $request->post('postal') ?: '',
                'nationality' => $request->post('nationality') ?: '',
                'dancerName' => $request->post('dancer-name') ?: '',
                'skupina' => $request->post('skupina') ?: '',
                'other' => $request->post('other') ?: ''
            ]);
        }

        Session::register(
            strtolower($request->post('username')),
            $request->post('pass'),
            $request->post('jmeno'),
            $request->post('prijmeni'),
            $request->post('pohlavi'),
            $request->post('email'),
            $request->post('telefon'),
            $narozeni,
            $poznamky,
            $request->post('street'),
            $request->post('popisne'),
            $request->post('orientacni'),
            $request->post('district'),
            $request->post('city'),
            $request->post('postal'),
            $request->post('nationality'),
            $request->post('skupina'),
            $request->post('poznamky') === 'dancer'
        );
        new \MessageHelper('success', 
            '<h4 class="alert-heading">Registrace úspěšně proběhla.</h4>' .
            '<p>Během několika dnů vám na email příjde potvrzení vašeho účtu, ' .
            'které vyřizuje administrátor ručně.<p>'
        );
        new \RedirectHelper('/');
    }
}
