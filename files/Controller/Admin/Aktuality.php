<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Aktuality extends Controller_Admin
{
    public function __construct() {
        Permissions::checkError('aktuality', P_OWNED);
    }
    public function view($id = null) {
        switch(post("action")) {
            case 'edit':
                $aktuality = post('aktuality');
                if ($aktuality[0])
                    $this->redirect('/admin/aktuality/edit/' . $aktuality[0]);
                break;
            case 'foto':
                $aktuality = post('aktuality');
                if ($aktuality[0])
                    $this->redirect('/admin/aktuality/foto/' . $aktuality[0]);
                break;
            case 'remove':
                if (!is_array(post('aktuality')))
                    break;
                $url = '/admin/aktuality/remove?';
                foreach (post('aktuality') as $id)
                    $url .= '&u[]=' . $id;
                $this->redirect($url);
                break;
        }
        $data = array_map(
            function($item) {
                return array(
                    'checkBox' => $this->checkbox('aktuality[]', $item['at_id'])->render(),
                    'name' => $item['at_jmeno'],
                    'category' => ($item['at_kat'] == AKTUALITY_CLANKY ? 'Články' :
                                   ($item['at_kat'] == AKTUALITY_VIDEA ? 'Videa' :
                                    ($item['at_kat'] == AKTUALITY_KRATKE ? 'Krátké zprávy' :
                                     '')))
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
    public function add($id = null) {
        if (empty($_POST)) {
            $this->render(
                'files/View/Admin/Aktuality/Form.inc',
                array(
                    'action' => Request::getAction()
                )
            );
            return;
        }

        $id = DBAktuality::addAktualita(
            User::getUserID(), post('kat'), post('jmeno'),
            post('text'), post('summary'), '0', '0'
        );

        if (post('action') == 'save') {
            $n = new Novinky(User::getUserID());
            switch(post('kat')) {
                case AKTUALITY_VIDEA:
                    $n->video()->add('/aktualne/' . $id, post('jmeno'));
                    break;
                case AKTUALITY_CLANKY:
                    $n->clanek()->add('/aktualne/' . $id, post('jmeno'));
                    break;
            }
            $this->redirect('/admin/aktuality', 'Článek přidán');
        } else {
            $this->redirect('/admin/aktuality/foto/' . $id . '?notify=true', 'Uloženo');
        }
    }
    public function edit($id = null) {
        if (!$id || !($data = DBAktuality::getSingleAktualita($id)))
            $this->redirect('/admin/aktuality', 'Článek s takovým ID neexistuje');

        Permissions::checkError('aktuality', P_OWNED, $data['at_kdo']);

        if (empty($_POST)) {
            post('kat', $data['at_kat']);
            post("jmeno", $data["at_jmeno"]);
            post('summary', $data['at_preview']);
            post("text", stripslashes($data["at_text"]));

            $this->render("files/View/Admin/Aktuality/Form.inc");
            return;
        }
        if (post('kat') != $data['at_kat'] || post('jmeno') != $data['at_jmeno']
            || post('text') != $data['at_text'] || post('summary') != $data['at_preview']) {
            DBAktuality::editAktualita(
                $id, post('kat'), post('jmeno'), post('text'),
                post('summary'), $data['at_foto'], $data['at_foto_main']
            );
            $changed = true;
        }
        if (isset($changed) && $changed) {
            $n = new Novinky(User::getUserID());
            switch(post('kat')) {
                case AKTUALITY_VIDEA:
                    $n->video()->edit('/aktualne/' . $id, post('jmeno'));
                    break;
                case AKTUALITY_CLANKY:
                    $n->clanek()->edit('/aktualne/' . $id, post('jmeno'));
                    break;
            }
        }
        $this->redirect('/admin/aktuality', 'Článek změněn');
    }
    public function remove($id = null)
    {
        if (!is_array(post('data')) && !is_array(get('u'))) {
            $this->redirect('/admin/aktuality');
        }
        if (!empty($_POST) && post('action') == 'confirm') {
            foreach (post('aktuality') as $id) {
                $data = DBAktuality::getSingleAktualita($id);
                
                if (Permissions::check('aktuality', P_OWNED, $data['at_kdo'])) {
                    DBAktuality::removeAktualita($id);
                    $n = new Novinky(User::getUserID());
                    switch($data['at_kat']) {
                        case AKTUALITY_VIDEA:
                            $n->video()->remove($data['at_jmeno']);
                            break;
                        case AKTUALITY_CLANKY:
                            $n->clanek()->remove($data['at_jmeno']);
                            break;
                    }
                } else {
                    $error = true;
                }
            }
            if (isset($error) && $error) {
                throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
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
                'returnURI' => Request::getReferer(),
                'data' => $data
            )
        );
    }
}
