<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Akce extends Controller_Admin
{
    function __construct()
    {
        Permissions::checkError('akce', P_OWNED);
    }
    function view($id = null)
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

        case 'edit':
        case 'detail':
        case 'dokumenty':
            $akce = post('akce');
            if ($akce[0]) {
                $this->redirect('/admin/akce/' . post('action') . '/' . $akce[0]);
            }
            break;
        }
        $this->_displayOverview();
    }
    function add($id = null)
    {
        if (empty($_POST) || is_object($form = $this->_checkData())) {
            if (empty($_POST)) {
                $form = array();
            }
            $this->_displayForm(null, $form);
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
    function edit($id = null)
    {
        if (!$id || !($data = DBAkce::getSingleAkce($id))) {
            $this->redirect('/admin/akce', 'Akce s takovým ID neexistuje');
        }
        if (empty($_POST) || is_object($form = $this->_checkData())) {
            if (empty($_POST)) {
                post('id', $id);
                post('jmeno', $data['a_jmeno']);
                post('kde', $data['a_kde']);
                post('info', $data['a_info']);
                post('od', $data['a_od']);
                post('do', $data['a_do']);
                post('kapacita', $data['a_kapacita']);
                post('lock', $data['a_lock']);
                post('visible', $data['a_visible']);
                $form = array();
            }
            $this->_displayForm($data, $form);
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
    function remove($id = null)
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
                'returnURL' => Request::getReferer(),
                'data' => $data
            )
        );
    }
    private function _displayOverview()
    {
        $currentId = 0;
        $currentIndex = -1;
        $data = array();
        $akce = DBAkce::getWithItems();
        foreach ($akce as $key => $item) {
            if($item['a_id'] == $currentId) {
                $data[$currentIndex]['userCount']++;
                continue;
            }
            $currentId = $item['a_id'];
            $currentIndex++;
            $data[$currentIndex] = array(
                'checkBox'  => getCheckbox('akce[]', $item['a_id']),
                'name'      => $item['a_jmeno'],
                'place'     => $item['a_kde'],
                'date'      => formatDate($item['a_od'])
                    . (($item['a_od'] != $item['a_do'])
                    ? ' - ' . formatDate($item['a_do']) : ''),
                'userCount' => 1,
                'visible'   => getCheckbox($item['a_id'], '1', $item['a_visible'])
            );
        }
        $this->render(
            'files/View/Admin/Akce/Overview.inc',
            array(
        	   'data' => $data
            )
        );
    }
    private function _displayForm($data, $form)
    {
        if(!$data || !is_array($dokumenty = unserialize($data['a_dokumenty']))) {
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
                'form' => $form,
                'dokumenty' => $dokumenty
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

    /**
     * Checks POST input
     * @see View_Admin_Akce_Overview
     * @return Form|array Returns empty array if form is valid
     */
    private function _checkData()
    {
        $od = $this->date('od')->getPost();
        $do = $this->date('do')->getPost();
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0)
            $do = $od;

        $form = new Form();
        $form->checkLength(post('jmeno'), 1, 255, 'Špatná délka jména akce', 'jmeno');
        $form->checkLength(post('kde'), 1, 255, 'Špatná délka místa konání', 'kde');
        $form->checkDate((string) $od, 'Špatný formát data ("Od")', 'od');
        if ($od != $do) {
            $form->checkDate((string) $do, 'Špatný formát data ("Do")', 'do');
        }
        $form->checkNumeric(post('kapacita'), 'Kapacita musí být zadána číselně', 'kapacita');

        return $form->isValid() ? array() : $form;
    }
}
?>