<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Video_Source extends Controller_Admin
{
    public function __construct()
    {
        Permissions::checkError('aktuality', P_OWNED);
    }

    public function view($request)
    {
        $this->render('files/View/Admin/VideoSource/Overview.inc', [
            'header' => 'Správa zdrojů videa',
            'data' => array_map(
                function ($item) {
                    return [
                        'buttons' => $this->editLink('/admin/video/source/edit/' . $item['vs_id'])
                        . '&nbsp;'
                        . $this->removeLink('/admin/video/source/remove/' . $item['vs_id']),
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
            $this->displayForm($request);
            return;
        }

        $form = $this->checkData($request);
        if (!$form->isValid()) {
            $this->redirect()->warning($form->getMessages());
            $this->displayForm($request);
            return;
        }

        DBVideoSource::add(
            $request->post('uri'),
            $request->post('title'),
            $request->post('desc'),
            false
        );

        $this->redirect('/admin/video/source');
    }

    public function edit($request)
    {
        $id = $request->getId();
        $data = DBVideoSource::getSingle($id);
        if (!$id || !$data) {
            $this->redirect()->warning('Článek s takovým ID neexistuje');
            $this->redirect('/admin/video/source');
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

        DBVideoSource::edit(
            $id,
            $request->post('uri'),
            $request->post('title'),
            $request->post('desc'),
            false
        );

        $this->redirect('/admin/video/source');
    }

    public function remove($request)
    {
        if (!$request->getId()) {
            $this->redirect('/admin/video/source');
        }

        if ($request->post('action') == 'confirm') {
            DBVideoSource::remove($request->getId());
            $this->redirect()->info('Video odebráno');
            $this->redirect('/admin/video/source');
        }

        $item = DBVideo::getSingle($request->getId());
        $this->render('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa videí',
            'prompt' => 'Opravdu chcete odstranit zdroj:',
            'returnURI' => $request->getReferer() ?: '/admin/video/source',
            'data' => [['id' => $item['vs_id'], 'text' => $item['vs_title']]]
        ]);
    }

    protected function displayForm($request, $data = [])
    {
        $this->render('files/View/Admin/VideoSource/Form.inc', [
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
