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
        switch(post('action')) {
        case 'remove':
            if (!is_array(post('aktuality'))) {
                $this->redirect('/admin/aktuality');
                break;
            }
            $url = '/admin/aktuality/remove?';
            foreach (post('aktuality') as $id) {
                $url .= '&u[]=' . $id;
            }
            $this->redirect($url);
            break;
        }
        $data = array_map(
            function ($item) {
                $editable = Permissions::check('aktuality', P_OWNED, $item['at_kdo']);
                return array(
                    'checkBox' => $this->checkbox('aktuality[]', $item['at_id'])
                                       ->readonly($editable),
                    'name' => $item['at_jmeno'],
                    'category' => ($item['at_kat'] == AKTUALITY_CLANKY
                                   ? 'Články'
                                   : ($item['at_kat'] == AKTUALITY_KRATKE
                                      ? 'Krátké zprávy'
                                      : '')),
                    'links' => $editable
                    ? ('<a href="/admin/aktuality/edit/' . $item['at_id'] . '">obecné</a>, ' .
                       '<a href="/admin/aktuality/foto/' . $item['at_id'] . '">galerie</a>')
                    : ''
                );
            },
            DBAktuality::getAktuality(get('f'))
        );
        $this->render(
            'files/View/Admin/Aktuality/Overview.inc',
            array(
                'data' => $data,
                'showMenu' => !TISK
            )
        );
    }
    public function add($request)
    {
        if (empty($_POST)) {
            $this->render(
                'files/View/Admin/Aktuality/Form.inc',
                array(
                    'action' => $request->getAction()
                )
            );
            return;
        }

        $id = DBAktuality::addAktualita(
            User::getUserID(), post('kat'), post('jmeno'),
            post('text'), post('summary'), '0', '0'
        );

        if (post('action') == 'save') {
            $news = new Novinky(User::getUserID());
            $news->clanek()->add('/aktualne/' . $id, post('jmeno'));
            $this->redirect('/admin/aktuality', 'Článek přidán');
        } else {
            $this->redirect('/admin/aktuality/foto/' . $id . '?notify=true', 'Uloženo');
        }
    }

    public function edit($request)
    {
        $id = $request->getId();
        if (!$id || !($data = DBAktuality::getSingleAktualita($id))) {
            $this->redirect('/admin/aktuality', 'Článek s takovým ID neexistuje');
        }

        Permissions::checkError('aktuality', P_OWNED, $data['at_kdo']);

        if (empty($_POST)) {
            $this->render(
                'files/View/Admin/Aktuality/Form.inc',
                array(
                    'action' => $request->getAction(),
                    'category' => $data['at_kat'],
                    'name' => $data['at_jmeno'],
                    'summary' => $data['at_preview'],
                    'text' => $data['at_text']
                )
            );
            return;
        }
        if (post('kat') != $data['at_kat']
            || post('jmeno') != $data['at_jmeno']
            || post('text') != $data['at_text']
            || post('summary') != $data['at_preview']
        ) {
            DBAktuality::editAktualita(
                $id,
                post('category'),
                post('name'),
                post('text'),
                post('summary'),
                $data['at_foto'],
                $data['at_foto_main']
            );
            $changed = true;
        }
        if (isset($changed) && $changed) {
            $news = new Novinky(User::getUserID());
            $news->clanek()->edit('/aktualne/' . $id, post('jmeno'));
        }
        $this->redirect('/admin/aktuality', 'Článek změněn');
    }

    public function remove($request)
    {
        if (!is_array(post('data')) && !is_array(get('u'))) {
            $this->redirect('/admin/aktuality');
        }
        if (!empty($_POST) && post('action') == 'confirm') {
            foreach (post('aktuality') as $id) {
                $data = DBAktuality::getSingleAktualita($id);

                if (Permissions::check('aktuality', P_OWNED, $data['at_kdo'])) {
                    DBAktuality::removeAktualita($id);
                    $news = new Novinky(User::getUserID());
                    $news->clanek()->remove($data['at_jmeno']);
                } else {
                    $error = true;
                }
            }
            if (isset($error) && $error) {
                throw new AuthorizationException('Máte nedostatečnou autorizaci pro tuto akci!');
            }
            $this->redirect('/admin/aktuality', 'Články odebrány');
        }

        $data = array();
        foreach (get('u') as $id) {
            $item = DBAktuality::getSingleAktualita($id);
            $data[] = array(
                'id' => $item['at_id'],
                'text' => $item['at_jmeno']
            );
        }
        $this->render(
            'files/View/Admin/RemovePrompt.inc',
            array(
                'header' => 'Správa aktualit',
                'prompt' => 'Opravdu chcete odstranit články:',
                'returnURI' => $request->getReferer(),
                'data' => $data
            )
        );
    }
}
