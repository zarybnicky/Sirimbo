<?php
class Controller_Admin_Video extends Controller_Admin
{
    public function __construct()
    {
        parent::__construct();
        Permissions::checkError('aktuality', P_OWNED);
    }

    public function view($request)
    {
        $this->redirect('/admin/video/orphan');
    }

    public function playlist($request)
    {
        if ($request->getId()) {
            $list = DBVideoList::getSingle($request->getId());
            $data = array_map(
                function ($item) {
                    $parts = explode('?', $item['v_uri']);
                    $uri = array_shift($parts);
                    return [
                        'buttons' => $this->editLink('/admin/video/edit/' . $item['v_id'])
                            . '&nbsp;'
                            . $this->removeLink('/admin/video/remove/' . $item['v_id']),
                        'title' => $item['v_title'],
                        'uri' => $uri,
                        'created' => formatTimestamp($item['v_created_at'], true)
                    ];
                },
                DBVideo::getByPlaylist($request->getId())
            );
            $this->render('files/View/Admin/Video/Overview.inc', [
                'header' => 'Správa videí',
                'data' => $data,
                'action' => 'playlist',
                'subheader' => $list['vl_title'],
                'navigation' => ''
            ]);
        } else {
            $data = array_map(
                function ($item) {
                    return [
                        'buttons' => $this->editLink('/admin/video/playlist/' . $item['vl_id']),
                        'title' => $item['vl_title'],
                        'uri' => $item['vl_url'],
                        'created' => formatTimestamp($item['vl_created_at'], true)
                    ];
                },
                DBVideoList::getAll()
            );
            $this->render('files/View/Admin/Video/Overview.inc', [
                'header' => 'Správa videí',
                'data' => $data,
                'action' => 'playlist',
                'navigation' => ''
            ]);
        }
    }

    public function orphan($request)
    {
        $pager = new Paging('DBVideo', 'orphan');
        $pager->setItemsPerPage($request->get('c'));
        $pager->setCurrentPage($request->get('p'));
        $data = array_map(
            function ($item) {
                $parts = explode('?', $item['v_uri']);
                $uri = array_shift($parts);
                return [
                    'buttons' => $this->editLink('/admin/video/edit/' . $item['v_id'])
                        . '&nbsp;'
                        . $this->removeLink('/admin/video/remove/' . $item['v_id']),
                    'title' => $item['v_title'],
                    'uri' => $uri,
                    'created' => formatTimestamp($item['v_created_at'], true)
                ];
            },
            $pager->getItems()
        );
        $this->render('files/View/Admin/Video/Overview.inc', [
            'header' => 'Správa videí',
            'data' => $data,
            'action' => 'orphan',
            'navigation' => $pager->getNavigation($request->get())
        ]);
    }

    public function title($request)
    {
        if ($request->post('video1')) {
            DBParameters::set('title_video1', $request->post('video1'));
            DBParameters::set('title_video2', $request->post('video2'));
            DBParameters::set('title_video3', $request->post('video3'));
            DBParameters::set('title_video4', $request->post('video4'));
            $this->redirect('/admin/video/title');
        }
        $select = $this->select()->optionsAssoc(DBVideo::getAll(), 'v_id', 'v_title');
        $this->render('files/View/Admin/Video/Title.inc', [
            'header' => 'Správa videí',
            'video1' => $select->name('video1')->set(DBParameters::get('title_video1'))->render(),
            'video2' => $select->name('video2')->set(DBParameters::get('title_video2'))->render(),
            'video3' => $select->name('video3')->set(DBParameters::get('title_video3'))->render(),
            'video4' => $select->name('video4')->set(DBParameters::get('title_video4'))->render()
        ]);
    }

    public function add($request)
    {
        if (!$request->post()) {
            $this->displayForm($request);
            return;
        }

        $form = $this->checkData($request);
        if (!$form->isValid()) {
            $this->redirect()->warning($form->getMessages());
            $this->displayForm($request);
            return;
        }

        DBVideo::add(
            $request->post('uri'),
            $request->post('title'),
            $request->post('author'),
            $request->post('desc'),
            $request->post('playlist') ?: null
        );

        $this->redirect('/admin/video');
    }

    public function edit($request)
    {
        $id = $request->getId();
        $data = DBVideo::getSingle($id);
        if (!$id || !$data) {
            $this->redirect()->warning('Článek s takovým ID neexistuje');
            $this->redirect('/admin/aktuality');
        }

        if (!$request->post()) {
            $this->displayForm($request, $data);
            return;
        }

        $form = $this->checkData($request);
        if (!$form->isValid()) {
            $this->redirect()->warning($form->getMessages());
            $this->displayForm($request, $data);
            return;
        }

        DBVideo::edit(
            $id,
            $request->post('uri'),
            $request->post('title'),
            $request->post('author'),
            $request->post('desc'),
            $request->post('playlist') ?: null
        );

        $this->redirect('/admin/video');
    }

    public function remove($request)
    {
        if (!$request->getId()) {
            $this->redirect('/admin/video');
        }

        if ($request->post('action') == 'confirm') {
            DBVideo::remove($request->getId());
            $this->redirect()->info('Video odebráno');
            $this->redirect('/admin/video');
        }

        $item = DBVideo::getSingle($request->getId());
        $this->render('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa videí',
            'prompt' => 'Opravdu chcete odstranit video:',
            'returnURI' => $request->getReferer() ?: '/admin/video',
            'data' => [['id' => $item['v_id'], 'text' => $item['v_title']]]
        ]);
    }

    protected function displayForm($request, $data = [])
    {
        $this->render('files/View/Admin/Video/Form.inc', [
            'header' => 'Správa videí',
            'subheader' => $request->getAction() == 'add' ? 'Přidat video' : 'Upravit video',
            'action' => $request->getAction() == 'add' ? 'Přidat' : 'Upravit',
            'id' => $data ? $data['v_id'] : null,
            'uri' => $request->post('uri') ?: ($data ? $data['v_uri'] : ''),
            'title' => $request->post('title') ?: ($data ? $data['v_title'] : ''),
            'author' => $request->post('author') ?: ($data ? $data['v_author'] : ''),
            'desc' => $request->post('desc') ?: ($data ? $data['v_description'] : ''),
            'playlist' => $request->post('playlist') ?: ($data ? ($data['v_playlist'] ?: '') : '')
        ]);
    }

    protected function checkData($request)
    {
        $form = new Form();
        $form->checkNotEmpty(
            $request->post('uri'),
            'Zadejce prosím ID videa',
            'uri'
        );
        return $form;
    }
}