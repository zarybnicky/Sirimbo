<?php
class Controller_Admin_Video_Source extends Controller_Abstract
{
    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('aktuality', P_OWNED);
    }

    public function view($request)
    {
        new \RenderHelper('files/View/Admin/VideoSource/Overview.inc', [
            'header' => 'Správa zdrojů videa',
            'data' => array_map(
                function ($item) {
                    return [
                        'buttons' => new EditLinkHelper('/admin/video/source/edit/' . $item['vs_id'])
                        . '&nbsp;'
                        . new RemoveLinkHelper('/admin/video/source/remove/' . $item['vs_id']),
                        'url' => $item['vs_url'],
                        'title' => $item['vs_title'],
                        'created' => formatTimestamp($item['vs_created_at'], true),
                        'lastChecked' => $item['vs_last_checked'] ? formatTimestamp($item['vs_last_checked'], true) : ''
                    ];
                },
                DBVideoSource::getAll()
            )
        ]);
    }

    public function add($request)
    {
        if (!$request->post()) {
            return $this->displayForm($request);
        }

        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->displayForm($request);
        }

        DBVideoSource::add($request->post('uri'));
        new \RedirectHelper('/admin/video/source');
    }

    public function edit($request)
    {
        $id = $request->getId();
        $data = DBVideoSource::getSingle($id);
        if (!$id || !$data) {
            new \MessageHelper('warning', 'Článek s takovým ID neexistuje');
            new \RedirectHelper('/admin/video/source');
        }

        if (!$request->post()) {
            return $this->displayForm($request, $data);
        }

        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->displayForm($request, $data);
        }

        DBVideoSource::edit(
            $id,
            $request->post('uri'),
            $request->post('title'),
            $request->post('desc')
        );

        new \RedirectHelper('/admin/video/source');
    }

    public function remove($request)
    {
        if (!$request->getId()) {
            new \RedirectHelper('/admin/video/source');
        }

        if ($request->post('action') == 'confirm') {
            DBVideoSource::remove($request->getId());
            new \MessageHelper('info', 'Video odebráno');
            new \RedirectHelper('/admin/video/source');
        }

        $item = DBVideo::getSingle($request->getId());
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa videí',
            'prompt' => 'Opravdu chcete odstranit zdroj:',
            'returnURI' => $request->getReferer() ?: '/admin/video/source',
            'data' => [['id' => $item['vs_id'], 'text' => $item['vs_title']]]
        ]);
    }

    protected function displayForm($request, $data = [])
    {
        new \RenderHelper('files/View/Admin/VideoSource/Form.inc', [
            'header' => 'Správa zdrojů videa',
            'subheader' => $request->getAction() == 'add' ? 'Přidat zdroj' : 'Upravit zdroj',
            'action' => $request->getAction() == 'add' ? 'Přidat' : 'Upravit',
            'id' => $data ? $data['vs_id'] : null,
            'uri' => $request->post('uri') ?: ($data ? $data['vs_url'] : ''),
            'title' => $request->post('title') ?: ($data ? $data['vs_title'] : ''),
            'desc' => $request->post('desc') ?: ($data ? $data['vs_description'] : '')
        ]);
    }

    protected function checkData($request)
    {
        $form = new Form();
        $form->checkNotEmpty(
            $request->post('uri'),
            'Zadejce prosím ID kanálu',
            'uri'
        );
        return $form;
    }
}
