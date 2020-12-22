<?php
namespace Olymp\Controller\Admin;

class VideoSource
{
    public static function list()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        new \RenderHelper('files/View/Admin/VideoSource/Overview.inc', [
            'header' => 'Správa zdrojů videa',
            'data' => array_map(
                fn($item) => [
                    'buttons' => \Buttons::videoSource($item['vs_id']),
                    'url' => $item['vs_url'],
                    'title' => $item['vs_title'],
                    'created' => \Format::timestamp($item['vs_created_at'], true),
                    'lastChecked' => $item['vs_last_checked']
                    ? \Format::timestamp($item['vs_last_checked'], true) : ''
                ],
                \DBVideoSource::getAll()
            )
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
        \DBVideoSource::add($_POST['uri']);
        \Redirect::to('/admin/video/source');
    }

    public static function edit($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!$data = \DBVideoSource::getSingle($id)) {
            new \MessageHelper('warning', 'Článek s takovým ID neexistuje');
            \Redirect::to('/admin/video/source');
        }
        return static::displayForm('edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!$data = \DBVideoSource::getSingle($id)) {
            new \MessageHelper('warning', 'Článek s takovým ID neexistuje');
            \Redirect::to('/admin/video/source');
        }
        $form = static::checkData();
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm('edit', $data);
        }
        \DBVideoSource::edit($id, $_POST['uri'], $_POST['title'], $_POST['desc']);
        \Redirect::to('/admin/video/source');
    }

    public static function remove($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $item = \DBVideo::getSingle($id);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa videí',
            'prompt' => 'Opravdu chcete odstranit zdroj:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/video/source',
            'data' => [['id' => $item['vs_id'], 'text' => $item['vs_title']]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        \DBVideoSource::remove($id);
        new \MessageHelper('info', 'Video odebráno');
        \Redirect::to('/admin/video/source');
    }

    protected static function displayForm($action, $data = [])
    {
        new \RenderHelper('files/View/Admin/VideoSource/Form.inc', [
            'header' => 'Správa zdrojů videa',
            'subheader' => $action == 'add' ? 'Přidat zdroj' : 'Upravit zdroj',
            'action' => $action == 'add' ? 'Přidat' : 'Upravit',
            'id' => $data ? $data['vs_id'] : null,
            'uri' => $_POST['uri'] ?? $data['vs_url'] ?? '',
            'title' => $_POST['title'] ?? $data['vs_title'] ?? '',
            'desc' => $_POST['desc'] ?? $data['vs_description'] ?? '',
        ]);
    }

    protected static function checkData()
    {
        $form = new \Form();
        $form->checkNotEmpty($_POST['uri'], 'Zadejte prosím ID kanálu', 'uri');
        return $form;
    }
}
