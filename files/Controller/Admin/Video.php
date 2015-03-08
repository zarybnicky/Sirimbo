<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Video extends Controller_Admin
{
    public function __construct()
    {
        Permissions::checkError('aktuality', P_OWNED);
    }

    public function view($request)
    {
        if ($request->post()) {
            if ($request->post('action') == 'remove'
                && $request->post('video')
            ) {
                $this->redirect(
                    '/admin/video/remove?' .
                    http_build_query(
                        array('u' => $request->post('video'))
                    )
                );
            } else {
                $this->redirect('/admin/video');
            }
        }
        $data = array_map(
            function ($item) {
                return array(
                    'checkBox' => $this->checkbox('video[]', $item['v_id'])
                                       ->render(),
                    'name' => $item['v_name'],
                    'text' => $item['v_text'],
                    'uri' => $item['v_uri'],
                    'date' => $item['v_date'],
                    'playlist' => $item['v_playlist'] ? 'ano' : 'ne',
                    'links' => '<a href="/admin/video/edit/' . $item['v_id'] . '">obecné</a>'
                );
            },
            DBVideo::getAll()
        );
        $this->render(
            'files/View/Admin/Video/Overview.inc',
            array(
                'data' => $data
            )
        );
    }

    public function add($request)
    {
        if (!$request->post()) {
            $this->displayForm($request);
            return;
        }

        $form = $this->checkData($request);
        if (!$form->isValid()) {
            $this->redirect()->setMessage($form->getMessages());
            $this->displayForm($request);
            return;
        }

        DBVideo::add(
            $request->post('name'),
            $request->post('text'),
            $request->post('uri'),
            false
        );

        $this->redirect('/admin/video', 'Video bylo přidáno');
    }

    public function edit($request)
    {
        $id = $request->getId();
        $data = DBVideo::getSingle($id);
        if (!$id || !$data) {
            $this->redirect('/admin/aktuality', 'Článek s takovým ID neexistuje');
        }

        if (!$request->post()) {
            $this->displayForm($request, $data);
            return;
        }

        $form = $this->checkData($request);
        if (!$form->isValid()) {
            $this->redirect()->setMessage($form->getMessages());
            $this->displayForm($request, $data);
            return;
        }

        DBVideo::edit(
            $id,
            $request->post('name'),
            $request->post('text'),
            $request->post('uri'),
            false
        );

        $this->redirect('/admin/video', 'Video bylo přidáno');
    }

    public function remove($request)
    {
        if (!$request->post('data') && !$request->get('u')) {
            $this->redirect('/admin/video');
        }

        if ($request->post('action') == 'confirm' && $request->post('data')) {
            foreach ($request->post('data') as $item) {
                DBVideo::remove($item);
            }
            $this->redirect('/admin/video', 'Videa odebrány');
        }

        if ($request->post()) {
            $this->redirect($request->server('REQUEST_URI'));
        }

        $data = array_map(
            function ($id) {
                $item = DBVideo::getSingle($id);
                return array(
                    'id' => $item['v_id'],
                    'text' => $item['v_name']
                );
            },
            $request->get('u')
        );

        $this->render(
            'files/View/Admin/RemovePrompt.inc',
            array(
                'header' => 'Správa videí',
                'prompt' => 'Opravdu chcete odstranit videa:',
                'returnURI' => $request->getReferer(),
                'data' => $data
            )
        );
    }

    protected function displayForm($request, $data = array())
    {
        $this->render(
            'files/View/Admin/Video/Form.inc',
            array(
                'header' => $request->getAction() == 'add' ? 'Přidat video' : 'Upravit video',
                'action' => $request->getAction() == 'add' ? 'Přidat' : 'Upravit',
                'id' => $data ? $data['v_id'] : null,
                'name' => $request->post('name') ?: ($data ? $data['v_name'] : ''),
                'text' => $request->post('text') ?: ($data ? $data['v_text'] : ''),
                'uri' => $request->post('uri') ?: ($data ? $data['v_uri'] : '')
            )
        );
    }

    protected function checkData($request)
    {
        $form = new Form();
        $form->checkNotEmpty(
            $request->post('name'),
            'Zadejte prosím název videa',
            'name'
        );
        $form->checkMaxLength(
            $request->post('name'),
            255,
            'Název videa může mít maximálně 255 znaků',
            'name'
        );
        $form->checkNotEmpty(
            $request->post('uri'),
            'Zadejce prosím URI videa',
            'uri'
        );
        $form->checkMaxLength(
            $request->post('uri'),
            255,
            'URI videa může mít maximálně 255 znaků',
            'uri'
        );
        return $form;
    }
}
