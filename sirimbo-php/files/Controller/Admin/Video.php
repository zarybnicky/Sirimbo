<?php
namespace Olymp\Controller\Admin;

class Video
{
    public static function orphan()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $pager = new \Paging(new \DBVideo(), 'orphan');
        $pager->setItemsPerPage($_GET['c'] ?? null);
        $pager->setCurrentPage($_GET['p'] ?? null);
        \Render::twig('Admin/Video.twig', [
            'action' => 'orphan',
            'navigation' => $pager->getNavigation(),
            'data' => array_map(
                fn($item) => [
                    'type' => 'video',
                    'id' => $item['v_id'],
                    'title' => $item['v_title'],
                    'uri' => explode('?', $item['v_uri'])[0],
                    'created' => $item['v_created_at'],
                ],
                $pager->getItems()
            ),
        ]);
    }

    public static function playlistList()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        \Render::twig('Admin/Video.twig', [
            'action' => 'playlist',
            'navigation' => '',
            'data' => array_map(
                fn($item) => [
                    'type' => 'playlist',
                    'id' => $item['vl_id'],
                    'title' => $item['vl_title'],
                    'uri' => $item['vl_url'],
                    'created' => $item['vl_created_at'],
                ],
                \DBVideoList::getAll()
            ),
        ]);
    }

    public static function playlist($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $list = \DBVideoList::getSingle($id);
        \Render::twig('Admin/Video.twig', [
            'playlist' => $list['vl_title'],
            'action' => 'playlist',
            'navigation' => '',
            'data' => array_map(
                fn($item) => [
                    'type' => 'video',
                    'id' => $item['v_id'],
                    'title' => $item['v_title'],
                    'uri' => explode('?', $item['v_uri'])[0],
                    'created' => $item['v_created_at'],
                ],
                \DBVideo::getByPlaylist($id)
            ),
        ]);
    }

    public static function title()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        \Render::twig('Admin/VideoTitle.twig', [
            'videos' => \DBVideo::getAll(),
            'video1' => \DBParameters::get('title_video1'),
            'video2' => \DBParameters::get('title_video2'),
            'video3' => \DBParameters::get('title_video3'),
            'video4' => \DBParameters::get('title_video4'),
        ]);
    }

    public static function titlePost()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        \DBParameters::set('title_video1', $_POST['video1']);
        \DBParameters::set('title_video2', $_POST['video2']);
        \DBParameters::set('title_video3', $_POST['video3']);
        \DBParameters::set('title_video4', $_POST['video4']);
        \Redirect::to('/admin/video/title');
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
            \Message::warning($form->getMessages());
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
            \Message::warning('Článek s takovým ID neexistuje');
            \Redirect::to('/admin/aktuality');
        }
        return static::displayForm('edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!$data = \DBVideo::getSingle($id)) {
            \Message::warning('Článek s takovým ID neexistuje');
            \Redirect::to('/admin/aktuality');
        }
        $form = static::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
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
        \Render::twig('RemovePrompt.twig', [
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
        \Message::info('Video odebráno');
        \Redirect::to('/admin/video');
    }

    protected static function displayForm($action, $data = [])
    {
        \Render::twig('Admin/VideoForm.twig', [
            'action' => $action,
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
