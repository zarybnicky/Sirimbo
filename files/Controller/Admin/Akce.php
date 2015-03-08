<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Akce extends Controller_Admin
{
    public function __construct()
    {
        Permissions::checkError('akce', P_OWNED);
    }

    public function view($request)
    {
        switch($request->post('action')) {
            case 'save':
                $this->processSave();
                $this->redirect('/admin/akce');
                break;

            case 'remove':
                if (!is_array($request->post('akce'))) {
                    $this->redirect('/admin/akce');
                } else {
                    $this->redirect(
                        '/admin/akce/remove?' .
                        http_build_query(
                            array('u' => $request->post('akce'))
                        )
                    );
                }
                break;
        }

        $this->displayOverview();
    }

    public function add($request)
    {
        if (!$request->post()) {
            $this->displayForm($request);
            return;
        }

        $form = $this->checkData($request);
        if (is_object($form)) {
            $this->redirect()->setMessage($form->getMessages());
            $this->displayForm($request);
            return;
        }

        $od = $this->date('od')->getPost($request);
        $do = $this->date('do')->getPost($request);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }
        DBAkce::addAkce(
            $request->post('jmeno'),
            $request->post('kde'),
            $request->post('info'),
            (string) $od,
            (string) $do,
            $request->post('kapacita'),
            $request->post('dokumenty'),
            ($request->post('lock') == 'lock') ? 1 : 0,
            $request->post('visible') ? '1' : '0'
        );

        $news = new Novinky(User::getUserID());
        $news->akce()->add('/member/akce', $request->post('jmeno'));

        $this->redirect('/admin/akce', 'Akce přidána');
    }

    public function edit($request)
    {
        $id = $request->getID();
        if (!$id || !($data = DBAkce::getSingleAkce($id))) {
            $this->redirect('/admin/akce', 'Akce s takovým ID neexistuje');
        }

        if (!$request->post()) {
            $this->displayForm($request, $data);
            return;
        }

        $form = $this->checkData();
        if (is_object($form)) {
            $this->redirect()->setMessage($form->getMessages());
            $this->displayForm($request, $data);
            return;
        }

        $od = $this->date('od')->getPost($request);
        $do = $this->date('do')->getPost($request);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }

        DBAkce::editAkce(
            $id,
            $request->post('jmeno'),
            $request->post('kde'),
            $request->post('info'),
            (string) $od,
            (string) $do,
            $request->post('kapacita'),
            $request->post('dokumenty'),
            ($request->post('lock') == 'lock') ? 1 : 0,
            $request->post('visible') ? '1' : '0'
        );

        $news = new Novinky(User::getUserID());
        $news->akce()->edit('/member/akce', $request->post('jmeno'));

        $this->redirect('/admin/akce', 'Akce upravena');
    }

    public function remove($request)
    {
        if (!is_array($request->post('data')) && !is_array($request->get('u'))) {
            $this->redirect('/admin/akce');
        }
        if ($request->post('action') == 'confirm') {
            foreach ($request->post('data') as $id) {
                $data = DBAkce::getSingleAkce($id);
                DBAkce::removeAkce($id);

                if (strcmp($data['a_do'], date('Y-m-d')) >= 0) {
                    $news = new Novinky(User::getUserID());
                    $news->akce()->remove($data['a_jmeno']);
                }
            }
            $this->redirect('/admin/akce', 'Akce odebrány');
            return;
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
                'returnURI' => $request->getReferer(),
                'data' => $data
            )
        );
    }

    private function displayOverview()
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
                                      ->set($item['a_visible'])
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

    private function displayForm($request, $data = array())
    {
        if ($data) {
            $dokumenty = array_map(
                function ($item) {
                    return array(
                        'id' => $item['d_id'],
                        'name' => $item['d_name']
                    );
                },
                DBDokumenty::getMultipleById(
                    unserialize($data['a_dokumenty'])
                )
            );
        } else {
            $dokumenty = array();
        }

        $this->render(
            'files/View/Admin/Akce/Form.inc',
            array(
                'dokumenty' => $dokumenty,
                'header' => $request->getAction() == 'add' ? 'Přidat uživatele' : 'Upravit uživatele',
                'action' => $request->getAction() == 'add' ? 'Přidat' : 'Upravit',
                'id' => $data ? $data['a_id'] : null,
                'jmeno' => $request->post('jmeno') ?: $data['a_jmeno'],
                'kde' => $request->post('kde') ?: $data['a_kde'],
                'info' => $request->post('info') ?: $data['a_info'],
                'od' => $request->post('od') ?: $data['a_od'],
                'do' => $request->post('do') ?: $data['a_do'],
                'kapacita' => $request->post('kapacita') ?: $data['a_kapacita'],
                'lock' => $request->post('lock') ?: $data['a_lock'],
                'visible' => $request->post('visible') ?: $data['a_visible']
            )
        );
    }

    private function processSave()
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

    private function checkData()
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
