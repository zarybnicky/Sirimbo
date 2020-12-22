<?php
namespace Olymp\Controller\Admin;

class Video
{
    public static function orphan()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $pager = new \Paging(new \DBVideo(), 'orphan');
        $pager->setItemsPerPage($_GET['c']);
        $pager->setCurrentPage($_GET['p']);
        $data = array_map(
            fn($item) => [
                'buttons' => \Buttons::video($item['v_id']),
                'title' => $item['v_title'],
                'uri' => explode('?', $item['v_uri'])[0],
                'created' => \Format::timestamp($item['v_created_at'], true)
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

    public static function playlistList()
    {
        $data = array_map(
            fn($item) => [
                'buttons' => \Buttons::edit('/admin/video/playlist/' . $item['vl_id']),
                'title' => $item['vl_title'],
                'uri' => $item['vl_url'],
                'created' => \Format::timestamp($item['vl_created_at'], true)
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

    public static function playlist($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $list = \DBVideoList::getSingle($id);
        $data = array_map(
            fn($item) => [
                'buttons' => \Buttons::video($item['v_id']),
                'title' => $item['v_title'],
                'uri' => explode('?', $item['v_uri'])[0],
                'created' => \Format::timestamp($item['v_created_at'], true)
            ],
            \DBVideo::getByPlaylist($id)
        );
        new \RenderHelper('files/View/Admin/Video/Overview.inc', [
            'header' => 'Správa videí',
            'data' => $data,
            'action' => 'playlist',
            'subheader' => $list['vl_title'],
            'navigation' => ''
        ]);
    }

    public static function title()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        \DBParameters::set('title_video1', $_POST['video1']);
        \DBParameters::set('title_video2', $_POST['video2']);
        \DBParameters::set('title_video3', $_POST['video3']);
        \DBParameters::set('title_video4', $_POST['video4']);
        \Redirect::to('/admin/video/title');
    }

    public static function titlePost()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $select = (new \SelectHelper())->optionsAssoc(\DBVideo::getAll(), 'v_id', 'v_title');
        new \RenderHelper('files/View/Admin/Video/Title.inc', [
            'header' => 'Správa videí',
            'video1' => (string) $select->name('video1')->set(\DBParameters::get('title_video1')),
            'video2' => (string) $select->name('video2')->set(\DBParameters::get('title_video2')),
            'video3' => (string) $select->name('video3')->set(\DBParameters::get('title_video3')),
            'video4' => (string) $select->name('video4')->set(\DBParameters::get('title_video4'))
        ]);
    }

    public static function add()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        return static::displayForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('aktuality', P_OWNED);
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
            $_POST['playlist'] ?? null
        );
        \Redirect::to('/admin/video');
    }

    public static function edit($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!$data = \DBVideo::getSingle($id)) {
            new \MessageHelper('warning', 'Článek s takovým ID neexistuje');
            \Redirect::to('/admin/aktuality');
        }
        return static::displayForm('edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!$data = \DBVideo::getSingle($id)) {
            new \MessageHelper('warning', 'Článek s takovým ID neexistuje');
            \Redirect::to('/admin/aktuality');
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
            $_POST['playlist'] ?? null
        );
        \Redirect::to('/admin/video');
    }

    public static function remove($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $item = \DBVideo::getSingle($id);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa videí',
            'prompt' => 'Opravdu chcete odstranit video:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/video',
            'data' => [['id' => $item['v_id'], 'text' => $item['v_title']]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        \DBVideo::remove($id);
        new \MessageHelper('info', 'Video odebráno');
        \Redirect::to('/admin/video');
    }

    protected static function displayForm($action, $data = [])
    {
        new \RenderHelper('files/View/Admin/Video/Form.inc', [
            'header' => 'Správa videí',
            'subheader' => $action == 'add' ? 'Přidat video' : 'Upravit video',
            'action' => $action == 'add' ? 'Přidat' : 'Upravit',
            'id' => $data ? $data['v_id'] : null,
            'uri' => $_POST['uri'] ?? $data['v_uri'] ?? '',
            'title' => $_POST['title'] ?? $data['v_title'] ?? '',
            'author' => $_POST['author'] ?? $data['v_author'] ?? '',
            'desc' => $_POST['desc'] ?? $data['v_description'] ?? '',
            'playlist' => $_POST['playlist'] ?? $data['v_playlist'] ?? '',
        ]);
    }

    protected static function checkData(): \Form
    {
        $form = new \Form();
        $form->checkNotEmpty($_POST['uri'], 'Zadejce prosím ID videa', 'uri');
        return $form;
    }
}
