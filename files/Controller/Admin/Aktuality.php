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
        if ($request->post('action') == 'remove') {
            if (!is_array($request->post('aktuality'))) {
                $this->redirect('/admin/aktuality');
                break;
            }
            $url = '/admin/aktuality/remove?';
            foreach ($request->post('aktuality') as $id) {
                $url .= '&u[]=' . $id;
            }
            $this->redirect($url);
            break;
        }

        if (Permissions::check('aktuality', P_ADMIN)) {
            $data = DBAktuality::getAktuality($request->get('f'));
        } else {
            $data = DBAktuality::getAktuality($request->get('f'), User::getUserID());
        }

        $data = array_map(
            function ($item) {
                $id = $item['at_id'];
                return [
                    'checkBox' => $this->checkbox('aktuality[]', $id)
                                       ->render(),
                    'name' => $item['at_jmeno'],
                    'category' => ($item['at_kat'] == AKTUALITY_CLANKY
                                   ? 'Články'
                                   : ($item['at_kat'] == AKTUALITY_KRATKE
                                      ? 'Krátké zprávy'
                                      : '')),
                    'links' => (
                        '<a href="/admin/aktuality/edit/' . $id . '">obecné</a>, '
                        . '<a href="/admin/aktuality/foto/' . $id . '">galerie</a>'
                    )
                ];
            },
            $data
        );
        $this->render(
            'files/View/Admin/Aktuality/Overview.inc',
            [
                'data' => $data,
                'f' => $request->get('f') ?: '',
                'showMenu' => !TISK
            ]
        );
    }
    public function add($request)
    {
        if (!$request->post()) {
            $this->render(
                'files/View/Admin/Aktuality/Form.inc',
                [
                    'action' => $request->getAction(),
                    'category' => '',
                    'name' => '',
                    'summary' => '',
                    'text' => ''
                ]
            );
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
        if (!is_array($request->post('data')) && !is_array($request->get('u'))) {
            $this->redirect('/admin/aktuality');
        }
        if ($request->post() && $request->post('action') == 'confirm') {
            foreach ($request->post('data') as $id) {
                $data = DBAktuality::getSingleAktualita($id);

                if (Permissions::check('aktuality', P_OWNED, $data['at_kdo'])) {
                    DBAktuality::removeAktualita($id);
                } else {
                    $error = true;
                }
            }
            if (isset($error) && $error) {
                throw new AuthorizationException('Máte nedostatečnou autorizaci pro tuto akci!');
            }
            $this->redirect('/admin/aktuality', 'Články odebrány');
        }

        $data = [];
        foreach ($request->get('u') as $id) {
            $item = DBAktuality::getSingleAktualita($id);
            $data[] = [
                'id' => $item['at_id'],
                'text' => $item['at_jmeno']
            ];
        }
        $this->render(
            'files/View/Admin/RemovePrompt.inc',
            [
                'header' => 'Správa aktualit',
                'prompt' => 'Opravdu chcete odstranit články:',
                'returnURI' => $request->getReferer() ?: '/admin/aktuality',
                'data' => $data
            ]
        );
    }
}
