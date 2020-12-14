<?php
class Controller_Admin_Aktuality extends Controller_Abstract
{
    public function view($request)
    {
        Permissions::checkError('aktuality', P_OWNED);
        $data = array_map(
            function ($item) {
                $id = $item['at_id'];
                return [
                    'name' => $item['at_jmeno'],
                    'added' => formatTimestamp($item['at_timestamp_add']),
                    'links' => (
                        '<a href="/admin/aktuality/edit/' . $id . '">obecné</a>, ' .
                        '<a href="/admin/aktuality/foto/' . $id . '">galerie</a>'
                    ),
                    'buttons' => new RemoveLinkHelper('/admin/aktuality/remove/' . $id)
                ];
            },
            Permissions::check('aktuality', P_ADMIN)
            ? DBAktuality::getAktuality(1)
            : DBAktuality::getAktuality(1, Session::getUserID())
        );
        new \RenderHelper('files/View/Admin/Aktuality/Overview.inc', [
            'header' => 'Správa aktualit',
            'data' => $data,
        ]);
    }

    public function add($request)
    {
        Permissions::checkError('aktuality', P_OWNED);
        if (!$request->post()) {
            return new \RenderHelper('files/View/Admin/Aktuality/Form.inc', [
                'header' => 'Správa aktualit',
                'subheader' => 'Přidat článek',
                'action' => $request->getAction(),
                'name' => '',
                'summary' => '',
                'text' => ''
            ]);
        }

        $id = DBAktuality::addAktualita(
            Session::getUserID(),
            1,
            $request->post('name'),
            $request->post('text'),
            $request->post('summary'),
            '0',
            '0'
        );

        if ($request->post('action') == 'save') {
            new \RedirectHelper('/admin/aktuality');
        } else {
            new \RedirectHelper('/admin/aktuality/foto/' . $id . '?notify=true');
        }
    }

    public function edit($request)
    {
        Permissions::checkError('aktuality', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Článek s takovým ID neexistuje');
            new \RedirectHelper('/admin/aktuality');
        }
        if (!$data = DBAktuality::getSingleAktualita($id)) {
            new \MessageHelper('warning', 'Článek s takovým ID neexistuje');
            new \RedirectHelper('/admin/aktuality');
        }

        Permissions::checkError('aktuality', P_OWNED, $data['at_kdo']);

        $date = DateTime::createFromFormat('j. n. Y H:i', $request->post('createdAt'));
        if (!$request->post() || $date === false) {
            if ($request->post() && $date === false) {
                new \MessageHelper('danger', 'Špatný formát data "Publikováno" (D. M. RRRR HH:SS)');
            }
            return new \RenderHelper('files/View/Admin/Aktuality/Form.inc', [
                'header' => 'Správa aktualit',
                'subheader' => 'Upravit článek',
                'action' => $request->getAction(),
                'name' => $request->post('name') ?: $data['at_jmeno'],
                'summary' => $request->post('summary') ?: $data['at_preview'],
                'text' => $request->post('text') ?: $data['at_text'],
                'createdAt' => $request->post('createdAt') ?: formatTimestamp($data['at_timestamp_add']),
            ]);
        }

        DBAktuality::editAktualita(
            $id,
            1,
            $request->post('name'),
            $request->post('text'),
            $request->post('summary'),
            $data['at_foto'],
            $data['at_foto_main'],
            $date->format('Y-m-d H:i:s')
        );
        new \RedirectHelper('/admin/aktuality');
    }

    public function remove($request)
    {
        Permissions::checkError('aktuality', P_OWNED);
        if (!$request->getId()) {
            new \RedirectHelper('/admin/aktuality');
        }
        $id = $request->getId();
        if ($request->post('action') == 'confirm') {
            $data = DBAktuality::getSingleAktualita($id);
            if (!Permissions::check('aktuality', P_OWNED, $data['at_kdo'])) {
                throw new AuthorizationException('Máte nedostatečnou autorizaci pro tuto akci!');
            }
            DBAktuality::removeAktualita($id);
            new \RedirectHelper('/admin/aktuality');
        }

        $item = DBAktuality::getSingleAktualita($id);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa aktualit',
            'prompt' => 'Opravdu chcete odstranit články:',
            'returnURI' => $request->getReferer() ?: '/admin/aktuality',
            'data' => [[
                'id' => $item['at_id'],
                'text' => $item['at_jmeno']
            ]]
        ]);
    }
}
