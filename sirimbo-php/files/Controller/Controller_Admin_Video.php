<?php
class Controller_Admin_Video
{
    public function view()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        new \RedirectHelper('/admin/video/orphan');
    }

    public function playlist($request)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if ($request->getId()) {
            $list = \DBVideoList::getSingle($request->getId());
            $data = array_map(
                fn($item) => [
                    'buttons' => (
                        new \EditLinkHelper('/admin/video/edit/' . $item['v_id']) . '&nbsp;' .
                        new \RemoveLinkHelper('/admin/video/remove/' . $item['v_id'])
                    ),
                    'title' => $item['v_title'],
                    'uri' => explode('?', $item['v_uri'])[0],
                    'created' => formatTimestamp($item['v_created_at'], true)
                ],
                \DBVideo::getByPlaylist($request->getId())
            );
            new \RenderHelper('files/View/Admin/Video/Overview.inc', [
                'header' => 'Správa videí',
                'data' => $data,
                'action' => 'playlist',
                'subheader' => $list['vl_title'],
                'navigation' => ''
            ]);
        } else {
            $data = array_map(
                fn($item) => [
                    'buttons' => new \EditLinkHelper('/admin/video/playlist/' . $item['vl_id']),
                    'title' => $item['vl_title'],
                    'uri' => $item['vl_url'],
                    'created' => formatTimestamp($item['vl_created_at'], true)
                ],
                \DBVideoList::getAll()
            );
            new \RenderHelper('files/View/Admin/Video/Overview.inc', [
                'header' => 'Správa videí',
                'data' => $data,
                'action' => 'playlist',
                'navigation' => ''
            ]);
        }
    }

    public function orphan()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $pager = new \Paging(new \DBVideo(), 'orphan');
        $pager->setItemsPerPage($_GET['c']);
        $pager->setCurrentPage($_GET['p']);
        $data = array_map(
            fn($item) => [
                'buttons' => (
                    new \EditLinkHelper('/admin/video/edit/' . $item['v_id']) . '&nbsp;' .
                    new \RemoveLinkHelper('/admin/video/remove/' . $item['v_id'])
                ),
                'title' => $item['v_title'],
                'uri' => explode('?', $item['v_uri'])[0],
                'created' => formatTimestamp($item['v_created_at'], true)
            ],
            $pager->getItems()
        );
        new \RenderHelper('files/View/Admin/Video/Overview.inc', [
            'header' => 'Správa videí',
            'data' => $data,
            'action' => 'orphan',
            'navigation' => $pager->getNavigation()
        ]);
    }

    public function title()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if ($_POST['video1']) {
            \DBParameters::set('title_video1', $_POST['video1']);
            \DBParameters::set('title_video2', $_POST['video2']);
            \DBParameters::set('title_video3', $_POST['video3']);
            \DBParameters::set('title_video4', $_POST['video4']);
            new \RedirectHelper('/admin/video/title');
        }
        $select = (new \SelectHelper())->optionsAssoc(\DBVideo::getAll(), 'v_id', 'v_title');
        new \RenderHelper('files/View/Admin/Video/Title.inc', [
            'header' => 'Správa videí',
            'video1' => $select->name('video1')->set(\DBParameters::get('title_video1')),
            'video2' => $select->name('video2')->set(\DBParameters::get('title_video2')),
            'video3' => $select->name('video3')->set(\DBParameters::get('title_video3')),
            'video4' => $select->name('video4')->set(\DBParameters::get('title_video4'))
        ]);
    }

    public function add()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!$_POST) {
            return static::displayForm('add');
        }

        $form = static::checkData();
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm('add');
        }

        \DBVideo::add(
            $_POST['uri'],
            $_POST['title'],
            $_POST['author'],
            $_POST['desc'],
            $_POST['playlist'] ?: null
        );

        new \RedirectHelper('/admin/video');
    }

    public function edit($request)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $id = $request->getId();
        $data = \DBVideo::getSingle($id);
        if (!$id || !$data) {
            new \MessageHelper('warning', 'Článek s takovým ID neexistuje');
            new \RedirectHelper('/admin/aktuality');
        }

        if (!$_POST) {
            return static::displayForm('edit', $data);
        }

        $form = static::checkData();
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm('edit', $data);
        }

        \DBVideo::edit(
            $id,
            $_POST['uri'],
            $_POST['title'],
            $_POST['author'],
            $_POST['desc'],
            $_POST['playlist'] ?: null
        );

        new \RedirectHelper('/admin/video');
    }

    public function remove($request)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!$request->getId()) {
            new \RedirectHelper('/admin/video');
        }

        if ($_POST['action'] == 'confirm') {
            \DBVideo::remove($request->getId());
            new \MessageHelper('info', 'Video odebráno');
            new \RedirectHelper('/admin/video');
        }

        $item = \DBVideo::getSingle($request->getId());
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa videí',
            'prompt' => 'Opravdu chcete odstranit video:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?: '/admin/video',
            'data' => [['id' => $item['v_id'], 'text' => $item['v_title']]]
        ]);
    }

    protected static function displayForm($action, $data = [])
    {
        new \RenderHelper('files/View/Admin/Video/Form.inc', [
            'header' => 'Správa videí',
            'subheader' => $action == 'add' ? 'Přidat video' : 'Upravit video',
            'action' => $action == 'add' ? 'Přidat' : 'Upravit',
            'id' => $data ? $data['v_id'] : null,
            'uri' => $_POST['uri'] ?: ($data ? $data['v_uri'] : ''),
            'title' => $_POST['title'] ?: ($data ? $data['v_title'] : ''),
            'author' => $_POST['author'] ?: ($data ? $data['v_author'] : ''),
            'desc' => $_POST['desc'] ?: ($data ? $data['v_description'] : ''),
            'playlist' => $_POST['playlist'] ?: ($data ? ($data['v_playlist'] ?: '') : '')
        ]);
    }

    protected static function checkData(): \Form
    {
        $form = new \Form();
        $form->checkNotEmpty($_POST['uri'], 'Zadejce prosím ID videa', 'uri');
        return $form;
    }
}
