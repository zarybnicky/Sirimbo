<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Aktuality extends Controller_Admin
{
    public function __construct()
    {
        Permissions::checkError('aktuality', P_OWNED);
    }

    public function view($request)
    {
        $data = Permissions::check('aktuality', P_ADMIN)
            ? DBAktuality::getAktuality($request->get('f'))
            : DBAktuality::getAktuality($request->get('f'), User::getUserID());

        $data = array_map(
            function ($item) {
                $id = $item['at_id'];
                return [
                    'name' => $item['at_jmeno'],
                    'category' => ($item['at_kat'] == AKTUALITY_CLANKY
                                   ? 'Články'
                                   : ($item['at_kat'] == AKTUALITY_KRATKE
                                      ? 'Krátké zprávy'
                                      : '')),
                    'added' => formatDate($item['at_timestamp_add']),
                    'links' => (
                        '<a href="/admin/aktuality/edit/' . $id . '">obecné</a>, ' .
                        '<a href="/admin/aktuality/foto/' . $id . '">galerie</a>'
                    ),
                    'buttons' => $this->removeLink('/admin/aktuality/remove/' . $id)
                ];
            },
            $data
        );
        $this->render('files/View/Admin/Aktuality/Overview.inc', [
            'data' => $data,
            'f' => $request->get('f') ?: '',
            'showMenu' => !TISK
        ]);
    }

    public function add($request)
    {
        if (!$request->post()) {
            $this->render('files/View/Admin/Aktuality/Form.inc', [
                'action' => $request->getAction(),
                'category' => '',
                'name' => '',
                'summary' => '',
                'text' => ''
            ]);
            return;
        }

        $id = DBAktuality::addAktualita(
            User::getUserID(),
            $request->post('category'),
            $request->post('name'),
            $request->post('text'),
            $request->post('summary'),
            '0',
            '0'
        );

        if ($request->post('action') == 'save') {
            $this->redirect('/admin/aktuality', 'Článek přidán');
        } else {
            $this->redirect(
                '/admin/aktuality/foto/' . $id . '?notify=true',
                'Uloženo'
            );
        }
    }

    public function edit($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBAktuality::getSingleAktualita($id))) {
            $this->redirect('/admin/aktuality', 'Článek s takovým ID neexistuje');
        }

        Permissions::checkError('aktuality', P_OWNED, $data['at_kdo']);

        if (!$request->post()) {
            $this->render(
                'files/View/Admin/Aktuality/Form.inc',
                [
                    'action' => $request->getAction(),
                    'category' => $data['at_kat'],
                    'name' => $data['at_jmeno'],
                    'summary' => $data['at_preview'],
                    'text' => $data['at_text']
                ]
            );
            return;
        }

        DBAktuality::editAktualita(
            $id,
            $request->post('category'),
            $request->post('name'),
            $request->post('text'),
            $request->post('summary'),
            $data['at_foto'],
            $data['at_foto_main']
        );
        $this->redirect('/admin/aktuality', 'Článek změněn');
    }

    public function remove($request)
    {
        if (!$request->getId()) {
            $this->redirect('/admin/aktuality');
        }
        $id = $request->getId();
        if ($request->post('action') == 'confirm') {
            $data = DBAktuality::getSingleAktualita($id);
            if (!Permissions::check('aktuality', P_OWNED, $data['at_kdo'])) {
                throw new AuthorizationException('Máte nedostatečnou autorizaci pro tuto akci!');
            }
            DBAktuality::removeAktualita($id);
            $this->redirect('/admin/aktuality', 'Článek odebrán');
        }

        $item = DBAktuality::getSingleAktualita($id);
        $this->render('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa aktualit',
            'prompt' => 'Opravdu chcete odstranit články:',
            'returnURI' => $request->getReferer() ?: '/admin/aktuality',
            'data' => [[
                'id' => $item['at_id'],
                'text' => $item['at_jmeno']
            ]]
        ]);
    }
}
