<?php
namespace TKOlomouc\Controller\Admin;

use TKOlomouc\Controller\Admin;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\Novinky;
use TKOlomouc\Utility\User;
use TKOlomouc\Model\DBAnkety;
use TKOlomouc\View\Exception\AuthorizationException;
use TKOlomouc\Utility\Request;

class Ankety extends Admin
{
    public function __construct()
    {
        Permissions::checkError('ankety', P_OWNED);
    }

    public function view($id = null)
    {
        switch(post('action')) {
            case 'save':
                $this->processSave();
                break;

            case 'edit':
                $ankety = post('ankety');
                if ($ankety[0]) {
                    $this->redirect('/admin/ankety/edit/' . $ankety[0]);
                }
                break;

            case 'remove':
                if (!is_array(post('ankety'))) break;
                $this->redirect(
                    '/admin/ankety/remove?' . http_build_query(array('u' => post('ankety')))
                );
                break;
        }
        $this->displayOverview();
    }

    public function add($id = null)
    {
        if (empty($_POST)) {
            $this->displayForm($data);
            return;
        }
        $visible = (bool) post('visible');
        if (!Permissions::check('ankety', P_ADMIN)) {
            $visible = false;
            $this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění ankety');
        }
        $newId = DBAnkety::addAnketa(
            User::getUserID(), post('jmeno'), post('text'), (bool) $visible
        );

        if ($visible) {
            $news = new Novinky(User::getUserID());
            $news->ankety()->add(post('jmeno'));
        }
        if (post('add_text')) {
            DBAnkety::addAnketaItem($newId, post('add_text'));
            unset($_POST['add_text']);
        }
        $this->redirect('/admin/ankety/edit/' . $newId, 'Anketa přidána');
    }

    public function edit($id = null)
    {
        if (!$id || !($data = DBAnkety::getSingleAnketa($id))) {
            $this->redirect('/admin/ankety', 'Anketa s takovým ID neexistuje');
        }
        Permissions::checkError('ankety', P_OWNED, $data['ak_kdo']);

        $items = DBAnkety::getAnketaItems($id);

        if (empty($_POST)) {
            post('jmeno', $data['ak_jmeno']);
            post('text', $data['ak_text']);
            post('visible', $data['ak_visible']);

            $this->displayForm($data);
            return;
        }

        if (post('remove') > 0) {
            DBAnkety::removeAnketaItem(post('remove'));
            $items = DBAnkety::getAnketaItems($id);
            $changed = true;
        }
        foreach ($items as $item) {
            if (post($item["aki_id"] . "-text") != $item["aki_text"]) {
                DBAnkety::editAnketaItem($item["aki_id"], post($item["aki_id"] . "-text"));
                $changed = true;
            }
        }
        $items = DBAnkety::getAnketaItems($id);

        if (post('add_text')) {
            DBAnkety::addAnketaItem($id, post('add_text'));
            post('add_text', null);
            $items = DBAnkety::getAnketaItems($id);
            $changed = true;
        }

        $visible = (bool) post('visible');
        $visible_prev = $data['ak_visible'];

        if (!Permissions::check('ankety', P_ADMIN) && $visible != $visible_prev) {
            $visible = $visible_prev;
            $this->redirect()->setMessage('Nemáte dostatečná oprávnění ke zviditelnění ankety');
        }

        if (
            $visible != $visible_prev
            || post('jmeno') != $data['ak_jmeno']
            || post('text') != $data['ak_text']
        ) {
            DBAnkety::editAnketa($id, post('jmeno'), post('text'), $visible);
            $data = DBAnkety::getSingleAnketa($id);
            $changed = true;
        }

        $news = new Novinky(User::getUserID());
        if (isset($changed) && $changed) {
            if ($visible) {
                if (!$visible_prev)
                    $news->ankety()->add($data['ak_jmeno']);
                else
                    $news->ankety()->edit($data['ak_jmeno']);
            } elseif (!$visible && $visible_prev) {
                    $news->ankety()->remove($data['ak_jmeno']);
            }
        }
        post('jmeno', $data['ak_jmeno']);
        post('text', $data['ak_text']);
        post('visible', $data['ak_visible']);

        $this->displayForm($data);
    }

    public function remove($id = null)
    {
        if (!is_array(post('data')) && !is_array(get('u'))) {
            $this->redirect('/admin/ankety');
        }
        if (!empty($_POST) && post('action') == 'confirm') {
            foreach (post('data') as $id) {
                $data = DBAnkety::getSingleAnketa($id);

                DBAnkety::removeAnketa($item);
                if ($data['ak_visible']) {
                    $n = new Novinky(User::getUserID());
                    $n->ankety()->remove($data['ak_jmeno']);
                }
            }
            $this->redirect('/admin/ankety', 'Ankety odebrány');
        }
        $data = array();
        foreach (get('u') as $id) {
            $item = DBAnkety::getSingleAnketa($id);

            $data[] = array(
                'id' => $item['ak_id'],
                'text' => $item['ak_jmeno']
            );
        }
        $this->render(
            'src/application/View/Admin/RemovePrompt.inc',
            array(
                'header' => 'Správa anket',
                'prompt' => 'Opravdu chcete odstranit ankety:',
                'returnURL' => Request::getReferer(),
                'data' => $data
            )
        );
    }

    private function displayOverview()
    {
        $data = DBAnkety::getAnkety();
        foreach ($data as &$item) {
            $newData = array(
                'id' => $item['ak_id'],
                'visible' => $item['ak_visible'],
                'jmeno' => $item['ak_jmeno;']
            );
            $item = $newData;
        }

        $this->render(
            'src/application/View/Admin/Ankety/Display.inc',
            array(
                'data' => $data
            )
        );
    }

    private function displayForm($data)
    {
        if (Request::getAction() != 'add') {
            $items = DBAnkety::getAnketaItems($data['ak_id']);
        } else {
            $items = array();
        }
        foreach ($items as &$item) {
            $newData = array(
                'id' => $item['aki_id'],
                'text' => $item['aki_text'],
                'pocet' => isset($item['aki_pocet']) ? (int) $item['aki_pocet'] : 0
            );
            $item = $newData;
        }
        $this->render(
            'src/application/View/Admin/Ankety/Form.inc',
            array(
                'action' => Request::getAction(),
                'canMakeVisible' => !Permissions::check('ankety', P_OWNED, $data['ak_kdo']),
                'items' => $items
            )
        );
    }

    private function processSave()
    {
        $items = DBAnkety::getAnkety();

        foreach ($items as $item) {
            if($post($item['ak_id']) == $item['ak_visible']) {
                continue;
            }
            DBAnkety::editAnketa(
                $item['ak_id'],
                $item['ak_jmeno'],
                $item['ak_text'],
                $item['ak_kdo'],
                post($item['ak_id']) ? '1' : '0'
            );
        }
    }
}
