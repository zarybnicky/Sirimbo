<?php
class Controller_Admin_Akce extends Controller_Abstract
{
    public function view($request)
    {
        Permissions::checkError('akce', P_OWNED);
        if ($request->post('action') == 'save') {
            $this->processSave($request);
            new \RedirectHelper('/admin/akce');
        }

        $data = array_map(
            function ($item) {
                return [
                    'name' => $item['a_jmeno'],
                    'date' => (
                        formatDate($item['a_od'])
                        . (($item['a_od'] != $item['a_do'])
                           ? ' - ' . formatDate($item['a_do']) : '')
                    ),
                    'userCount' => $item['a_obsazeno'] . '/' . $item['a_kapacita'],
                    'visible' => new \CheckboxHelper($item['a_id'], '1', $item['a_visible']),
                    'links' => (
                        '<a href="/admin/akce/edit/' . $item['a_id'] . '">obecné</a>, '
                        . '<a href="/admin/akce/detail/' . $item['a_id'] . '">účastníci</a>, '
                        . '<a href="/admin/akce/dokumenty/' . $item['a_id'] . '">dokumenty</a>'
                    ),
                    'buttons' => new RemoveLinkHelper('/admin/akce/remove/' . $item['a_id'])
                ];
            },
            DBAkce::getWithItemCount()
        );

        new \RenderHelper('files/View/Admin/Akce/Overview.inc', [
            'header' => 'Správa akcí',
            'data' => $data
        ]);
    }

    public function add($request)
    {
        Permissions::checkError('akce', P_OWNED);
        if (!$request->post()) {
            return $this->displayForm($request);
        }

        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->displayForm($request);
        }

        $od = new Date($_POST['od'] ?? null);
        $do = new Date($_POST['do'] ?? null);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }
        DBAkce::addAkce(
            $request->post('jmeno'),
            $request->post('kde'),
            $request->post('info'),
            (string) $od,
            (string) $do,
            $request->post('kapacita'),
            '',
            ($request->post('lock') == 'lock') ? 1 : 0,
            $request->post('visible') ? '1' : '0'
        );

        new \MessageHelper('success', 'Akce přidána');
        new \RedirectHelper('/admin/akce');
    }

    public function edit($request)
    {
        Permissions::checkError('akce', P_OWNED);
        if (!($id = $request->getID())) {
            new \MessageHelper('warning', 'Akce s takovým ID neexistuje');
            new \RedirectHelper('/admin/akce');
        }
        if (!($data = DBAkce::getSingleAkce($id))) {
            new \MessageHelper('warning', 'Akce s takovým ID neexistuje');
            new \RedirectHelper('/admin/akce');
        }

        if (!$request->post()) {
            return $this->displayForm($request, $data);
        }

        $form = $this->checkData($request);
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return $this->displayForm($request, $data);
        }

        $od = new Date($_POST['od'] ?? null);
        $do = new Date($_POST['do'] ?? null);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }

        DBAkce::editAkce(
            $id,
            $request->post('jmeno'),
            $request->post('kde'),
            $request->post('info'),
            (string) $od,
            (string) $do,
            $request->post('kapacita'),
            $data['a_dokumenty'],
            ($request->post('lock') == 'lock') ? 1 : 0,
            $request->post('visible') ? '1' : '0'
        );

        new \MessageHelper('success', 'Akce upravena');
        new \RedirectHelper('/admin/akce');
    }

    public function remove($request)
    {
        Permissions::checkError('akce', P_OWNED);
        if (!$request->getId()) {
            new \RedirectHelper('/admin/akce');
        }
        $id = $request->getId();
        if ($request->post('action') == 'confirm') {
            DBAkce::removeAkce($id);
            new \MessageHelper('success', 'Akce odebrány');
            return new \RedirectHelper('/admin/akce');
        }

        $item = DBAkce::getSingleAkce($id);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa akcí',
            'prompt' => 'Opravdu chcete odstranit akce:',
            'returnURI' => $request->getReferer() ?: '/admin/akce',
            'data' => [[
                'id' => $item['a_id'],
                'text' => $item['a_jmeno']
            ]]
        ]);
    }

    private function displayForm($request, $data = [])
    {
        if ($data) {
            $dokumenty = array_map(
                function ($item) {
                    return [
                        'id' => $item['d_id'],
                        'name' => $item['d_name']
                    ];
                },
                DBDokumenty::getMultipleById(
                    array_filter(explode(',', $data['a_dokumenty']))
                )
            );
        } else {
            $dokumenty = [];
        }

        new \RenderHelper('files/View/Admin/Akce/Form.inc', [
            'header' => 'Správa akcí',
            'subheader' => $request->getAction() == 'add' ? 'Přidat akci' : 'Upravit akci',
            'dokumenty' => $dokumenty,
            'action' => $request->getAction() == 'add' ? 'Přidat' : 'Upravit',
            'id' => $data ? $data['a_id'] : null,
            'jmeno' => $request->post('jmeno') ?: ($data ? $data['a_jmeno'] : ''),
            'kde' => $request->post('kde') ?: ($data ? $data['a_kde'] : ''),
            'info' => $request->post('info') ?: ($data ? $data['a_info'] : ''),
            'od' => $request->post('od') ?: ($data ? $data['a_od'] : ''),
            'do' => $request->post('do') ?: ($data ? $data['a_do'] : ''),
            'kapacita' => $request->post('kapacita') ?: ($data ? $data['a_kapacita'] : ''),
            'lock' => $request->post('lock') ?: ($data ? $data['a_lock'] : ''),
            'visible' => $request->post('visible') ?: ($data ? $data['a_visible'] : '')
        ]);
    }

    private function processSave($request)
    {
        $items = DBAkce::getAkce();
        foreach ($items as $item) {
            if ((bool) $request->post($item['a_id']) === (bool) $item['a_visible']) {
                continue;
            }
            DBAkce::editAkce(
                $item['a_id'],
                $item['a_jmeno'],
                $item['a_kde'],
                $item['a_info'],
                $item['a_od'],
                $item['a_do'],
                $item['a_kapacita'],
                $item['a_dokumenty'],
                $item['a_lock'],
                $request->post($item['a_id']) ? '1' : '0'
            );
        }
    }

    private function checkData($request): Form
    {
        $od = new Date($_POST['od'] ?? null);
        $do = new Date($_POST['do'] ?? null);

        $form = new Form();
        $form->checkDate((string) $od, 'Špatný formát data ("Od")', 'od');
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $form->checkDate((string) $do, 'Špatný formát data ("Do")', 'do');
        }
        $form->checkNumeric($request->post('kapacita'), 'Kapacita musí být zadána číselně', 'kapacita');

        return $form;
    }
}
