<?php
class Controller_Admin_Video_Source
{
    public function view()
    {
        \Permissions::checkError('aktuality', P_OWNED);
        new \RenderHelper('files/View/Admin/VideoSource/Overview.inc', [
            'header' => 'Správa zdrojů videa',
            'data' => array_map(
                fn($item) => [
                    'buttons' => new \EditLinkHelper('/admin/video/source/edit/' . $item['vs_id'])
                    . '&nbsp;'
                    . new \RemoveLinkHelper('/admin/video/source/remove/' . $item['vs_id']),
                    'url' => $item['vs_url'],
                    'title' => $item['vs_title'],
                    'created' => formatTimestamp($item['vs_created_at'], true),
                    'lastChecked' => $item['vs_last_checked'] ? formatTimestamp($item['vs_last_checked'], true) : ''
                ],
                \DBVideoSource::getAll()
            )
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

        \DBVideoSource::add($_POST['uri']);
        new \RedirectHelper('/admin/video/source');
    }

    public function edit($request)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        $id = $request->getId();
        $data = \DBVideoSource::getSingle($id);
        if (!$id || !$data) {
            new \MessageHelper('warning', 'Článek s takovým ID neexistuje');
            new \RedirectHelper('/admin/video/source');
        }

        if (!$_POST) {
            return static::displayForm('edit', $data);
        }

        $form = static::checkData();
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm('edit', $data);
        }

        \DBVideoSource::edit($id, $_POST['uri'], $_POST['title'], $_POST['desc']);

        new \RedirectHelper('/admin/video/source');
    }

    public function remove($request)
    {
        \Permissions::checkError('aktuality', P_OWNED);
        if (!$request->getId()) {
            new \RedirectHelper('/admin/video/source');
        }

        if ($_POST['action'] == 'confirm') {
            \DBVideoSource::remove($request->getId());
            new \MessageHelper('info', 'Video odebráno');
            new \RedirectHelper('/admin/video/source');
        }

        $item = \DBVideo::getSingle($request->getId());
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa videí',
            'prompt' => 'Opravdu chcete odstranit zdroj:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?: '/admin/video/source',
            'data' => [['id' => $item['vs_id'], 'text' => $item['vs_title']]]
        ]);
    }

    protected static function displayForm($action, $data = [])
    {
        new \RenderHelper('files/View/Admin/VideoSource/Form.inc', [
            'header' => 'Správa zdrojů videa',
            'subheader' => $action == 'add' ? 'Přidat zdroj' : 'Upravit zdroj',
            'action' => $action == 'add' ? 'Přidat' : 'Upravit',
            'id' => $data ? $data['vs_id'] : null,
            'uri' => $_POST['uri'] ?: ($data ? $data['vs_url'] : ''),
            'title' => $_POST['title'] ?: ($data ? $data['vs_title'] : ''),
            'desc' => $_POST['desc'] ?: ($data ? $data['vs_description'] : '')
        ]);
    }

    protected static function checkData()
    {
        $form = new \Form();
        $form->checkNotEmpty($_POST['uri'], 'Zadejte prosím ID kanálu', 'uri');
        return $form;
    }
}
