<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Akce extends Controller_Admin
{
    public function __construct()
    {
        Permissions::checkError('akce', P_OWNED);
    }
    public function view($id = null)
    {
        switch(post('action')) {
        case 'save':
            $this->_processSave();
            $this->redirect('/admin/akce');
            break;
        case 'remove':
            if (is_array(post('akce'))) {
                $this->redirect(
                    '/admin/akce/remove?' . http_build_query(array('u' => post('akce')))
                );
            }
            break;
        }
        
        $this->_displayOverview();
    }

    public function add($id = null)
    {
        if (empty($_POST) || is_object($form = $this->_checkData())) {
            if (!empty($_POST)) {
                $this->redirect()->setMessage($form->getMessages());
            }
            $this->_displayForm(null);
            return;
        }
        $od = $this->date('od')->getPost();
        $do = $this->date('do')->getPost();
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }
        DBAkce::addAkce(
            post('jmeno'), post('kde'), post('info'),
            (string) $od, (string) $do, post('kapacita'), post('dokumenty'),
            (post('lock') == 'lock') ? 1 : 0, post('visible') ? '1' : '0'
        );

        $n = new Novinky(User::getUserID());
        $n->akce()->add('/member/akce', post('jmeno'));

        $this->redirect('/admin/akce', 'Akce přidána');
    }

    public function edit($id = null)
    {
        if (!$id || !($data = DBAkce::getSingleAkce($id))) {
            $this->redirect('/admin/akce', 'Akce s takovým ID neexistuje');
        }
        if (empty($_POST) || is_object($form = $this->_checkData())) {
            if (!empty($_POST)) {
                $this->redirect()->setMessage($form->getMessages());
            } else {
                post('id', $id);
                post('jmeno', $data['a_jmeno']);
                post('kde', $data['a_kde']);
                post('info', $data['a_info']);
                post('od', $data['a_od']);
                post('do', $data['a_do']);
                post('kapacita', $data['a_kapacita']);
                post('lock', $data['a_lock']);
                post('visible', $data['a_visible']);
            }
            $this->_displayForm($data);
            return;
        }
        $od = $this->date('od')->getPost();
        $do = $this->date('do')->getPost();
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }
        DBAkce::editAkce(
            $id, post('jmeno'), post('kde'), post('info'),
            (string) $od, (string) $do, post('kapacita'), post('dokumenty'),
            (post('lock') == 'lock') ? 1 : 0, post('visible') ? '1' : '0'
        );

        $n = new Novinky(User::getUserID());
        $n->akce()->edit('/member/akce', post('jmeno'));

        $this->redirect('/admin/akce', 'Akce upravena');
    }

    public function remove($id = null)
    {
        if (!is_array(post('data')) && !is_array(get('u'))) {
            $this->redirect('/admin/akce');
        }
        if (!empty($_POST) && post('action') == 'confirm') {
            foreach (post('data') as $id) {
                $data = DBAkce::getSingleAkce($id);
                DBAkce::removeAkce($id);

                if (strcmp($data['a_do'], date('Y-m-d')) >= 0) {
                    $n = new Novinky(User::getUserID());
                    $n->akce()->remove($data['a_jmeno']);
                }
            }
            $this->redirect('/admin/akce', 'Akce odebrány');
        }
        $data = array();
        foreach (get('u') as $id) {
            $item = DBAkce::getSingleAkce($id);
            $data[] = array(
                'id' => $item['a_id'],
                'text' => $item['a_jmeno']
            );
        }
        $this->render(
            'files/View/Admin/RemovePrompt.inc',
            array(
                'header' => 'Správa akcí',
                'prompt' => 'Opravdu chcete odstranit akce:',
                'returnURI' => Request::getReferer(),
                'data' => $data
            )
        );
    }

    private function _displayOverview()
    {
        $data = array_map(
            function($item) {
                return array(
                    'checkBox' => $this->checkbox('akce[]', $item['a_id'])
                                       ->render(),
                    'name' => $item['a_jmeno'],
                    'date' => (
                        formatDate($item['a_od'])
                        . (($item['a_od'] != $item['a_do'])
                           ? ' - ' . formatDate($item['a_do']) : '')
                    ),
                    'userCount' => $item['a_obsazeno'] . '/' . $item['a_kapacita'],
                    'visible' => $this->checkbox($item['a_id'], '1')
                                      ->defaultState($item['a_visible'])
                                      ->render(),
                    'links' => (
                        '<a href="/admin/akce/edit/' . $item['a_id'] . '">obecné</a>, ' .
                        '<a href="/admin/akce/detail/' . $item['a_id'] . '">účastníci</a>, ' .
                        '<a href="/admin/akce/dokumenty/' . $item['a_id'] . '">dokumenty</a>'
                    )
                );
            },
            DBAkce::getWithItemCount()
        );

        $this->render(
            'files/View/Admin/Akce/Overview.inc',
            array(
               'data' => $data
            )
        );
    }

    private function _displayForm($data)
    {
        if (!$data || !is_array($dokumenty = unserialize($data['a_dokumenty']))) {
            $dokumenty = array();
        } else {
            $dokumenty = DBDokumenty::getMultipleById($dokumenty);
            foreach ($dokumenty as &$item) {
                $new_data = array(
                    'id' => $item['d_id'],
                    'name' => $item['d_name']
                );
                $item = $new_data;
            }
        }
        $this->render(
            'files/View/Admin/Akce/Form.inc',
            array(
                'dokumenty' => $dokumenty,
                'header' => Request::getAction() == 'add' ? 'Přidat uživatele' : 'Upravit uživatele',
                'action' => Request::getAction() == 'add' ? 'Přidat' : 'Upravit'
            )
        );
    }

    private function _processSave()
    {
        $items = DBAkce::getAkce();
        foreach ($items as $item) {
            if ((bool) post($item['a_id']) === (bool) $item['a_visible']) {
                continue;
            }
            DBAkce::editAkce(
                $item['a_id'], $item['a_jmeno'], $item['a_kde'],
                $item['a_info'], $item['a_od'],
                $item['a_do'], $item['a_kapacita'],
                $item['a_dokumenty'], $item['a_lock'],
                post($item['a_id']) ? '1' : '0'
            );
        }
    }

    private function _checkData()
    {
        $od = $this->date('od')->getPost();
        $do = $this->date('do')->getPost();

        $form = new Form();
        $form->checkLength(post('jmeno'), 1, 255, 'Špatná délka jména akce', 'jmeno');
        $form->checkLength(post('kde'), 1, 255, 'Špatná délka místa konání', 'kde');
        $form->checkDate((string) $od, 'Špatný formát data ("Od")', 'od');
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $form->checkDate((string) $do, 'Špatný formát data ("Do")', 'do');
        }
        $form->checkNumeric(post('kapacita'), 'Kapacita musí být zadána číselně', 'kapacita');

        return $form->isValid() ? array() : $form;
    }
}
