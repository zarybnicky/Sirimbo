<?php
namespace Olymp\Controller\Admin;

class Akce
{
    public static function list()
    {
        \Permissions::checkError('akce', P_OWNED);
        $data = array_map(
            fn($item) => [
                'name' => $item['a_jmeno'],
                'date' => \Format::date($item['a_od'])
                . (($item['a_od'] != $item['a_do']) ? ' - ' . \Format::date($item['a_do']) : ''),
                'userCount' => $item['a_obsazeno'] . '/' . $item['a_kapacita'],
                'visible' => new \CheckboxHelper($item['a_id'], '1', $item['a_visible']),
                'links' => (
                    '<a href="/admin/akce/edit/' . $item['a_id'] . '">obecné</a>, '
                    . '<a href="/admin/akce/detail/' . $item['a_id'] . '">účastníci</a>, '
                    . '<a href="/admin/akce/dokumenty/' . $item['a_id'] . '">dokumenty</a>'
                ),
                'buttons' => \Buttons::delete('/admin/akce/remove/' . $item['a_id'])
            ],
            \DBAkce::getWithItemCount()
        );

        new \RenderHelper('files/View/Admin/Akce/Overview.inc', [
            'header' => 'Správa akcí',
            'data' => $data
        ]);
    }

    public static function listPost()
    {
        \Permissions::checkError('akce', P_OWNED);
        $items = \DBAkce::getAkce();
        foreach ($items as $item) {
            if ((bool) $_POST[$item['a_id']] === (bool) $item['a_visible']) {
                continue;
            }
            \DBAkce::editAkce(
                $item['a_id'],
                $item['a_jmeno'],
                $item['a_kde'],
                $item['a_info'],
                $item['a_od'],
                $item['a_do'],
                $item['a_kapacita'],
                $item['a_dokumenty'],
                $item['a_lock'],
                $_POST[$item['a_id']] ? '1' : '0'
            );
        }
        \Redirect::to('/admin/akce');
    }

    public static function add()
    {
        \Permissions::checkError('akce', P_OWNED);
        return static::displayForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('akce', P_OWNED);
        $form = static::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm('add');
        }

        $od = new \Date($_POST['od'] ?? null);
        $do = new \Date($_POST['do'] ?? null);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }
        \DBAkce::addAkce(
            $_POST['jmeno'],
            $_POST['kde'],
            $_POST['info'],
            (string) $od,
            (string) $do,
            $_POST['kapacita'],
            '',
            ($_POST['lock'] == 'lock') ? 1 : 0,
            $_POST['visible'] ? '1' : '0'
        );

        \Message::success('Akce přidána');
        \Redirect::to('/admin/akce');
    }

    public static function edit($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        if (!($data = \DBAkce::getSingleAkce($id))) {
            \Message::warning('Akce s takovým ID neexistuje');
            \Redirect::to('/admin/akce');
        }
        return static::displayForm('edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        if (!($data = \DBAkce::getSingleAkce($id))) {
            \Message::warning('Akce s takovým ID neexistuje');
            \Redirect::to('/admin/akce');
        }

        $form = static::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm('edit', $data);
        }

        $od = new \Date($_POST['od'] ?? null);
        $do = new \Date($_POST['do'] ?? null);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }

        \DBAkce::editAkce(
            $id,
            $_POST['jmeno'],
            $_POST['kde'],
            $_POST['info'],
            (string) $od,
            (string) $do,
            $_POST['kapacita'],
            $data['a_dokumenty'],
            ($_POST['lock'] == 'lock') ? 1 : 0,
            $_POST['visible'] ? '1' : '0'
        );

        \Message::success('Akce upravena');
        \Redirect::to('/admin/akce');
    }

    public static function remove($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        $item = \DBAkce::getSingleAkce($id);
        new \RenderHelper('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa akcí',
            'prompt' => 'Opravdu chcete odstranit akce:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/akce',
            'data' => [[
                'id' => $item['a_id'],
                'text' => $item['a_jmeno']
            ]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        \DBAkce::removeAkce($id);
        \Message::success('Akce odebrány');
        \Redirect::to('/admin/akce');
    }

    public static function detail($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        if (!$akce = \DBAkce::getSingleAkce($id)) {
            \Message::warning('Akce s takovým ID neexistuje');
            \Redirect::to('/admin/akce');
        }
        $data = [
            'id' => $akce['a_id'],
            'jmeno' => $akce['a_jmeno'],
            'kde' => $akce['a_kde'],
            'datum' => \Format::date($akce['a_od'])
                . (($akce['a_od'] != $akce['a_do'])
                ? ' - ' . \Format::date($akce['a_do'])
                : ''),
            'kapacita' => $akce['a_kapacita'],
            'volno' => $akce['a_kapacita'] - count(\DBAkce::getAkceItems($id)),
            'showForm' => \Permissions::check('akce', P_MEMBER)
                && !$akce['a_lock'],
            'canEdit' => \Permissions::check('akce', P_OWNED),
            'info' => nl2br($akce['a_info'])
        ];

        $userSelect = new \UserSelectHelper(\DBUser::getActiveUsers());
        $items = array_map(
            fn($item) => [
                'name' => (string) $userSelect->name($item['ai_id'] . '-user')->set($item['ai_user']),
                'removeButton' => (new \SubmitHelper('Odstranit'))->data('remove', $item['ai_id'])
            ],
            \DBAkce::getAkceItems($id)
        );
        $items[] = [
            'name' => (string) $userSelect->name('add-user')->set(0),
            'removeButton' => (new \SubmitHelper('Přidat'))->data('add', 'add')
        ];

        new \RenderHelper('files/View/Admin/Akce/Detail.inc', [
            'header' => 'Správa akcí',
            'data' => $data,
            'items' => $items
        ]);
    }

    public static function detailPost($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        if (!\DBAkce::getSingleAkce($id)) {
            \Message::warning('Akce s takovým ID neexistuje');
            \Redirect::to('/admin/akce');
        }

        if (isset($_POST["remove"])) {
            \DBAkce::removeAkceItem($_POST["remove"]);
        }

        foreach (\DBAkce::getAkceItems($id) as $item) {
            $user = $_POST[$item["ai_id"] . '-user'];

            if (!$user) {
                \DBAkce::removeAkceItem($item['ai_id']);
            } elseif ($user != $item["ai_user"]) {
                $data = \DBUser::getUserData($user);
                list($year) = explode('-', $data['u_narozeni']);
                \DBAkce::editAkceItem($item["ai_id"], $user, $year);
            }
        }

        if (is_numeric($_POST["add-user"]) && $_POST['add-user'] > 0) {
            $user = $_POST["add-user"];
            $data = \DBUser::getUserData($user);
            list($year) = explode('-', $data['u_narozeni']);

            \DBAkce::addAkceItem($id, $user, $year);
            $_POST['add-user'] = 0;
        }
        \Redirect::to('/admin/akce/detail/' . $id);
    }

    public static function dokumenty($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        if (!($akce = \DBAkce::getSingleAkce($id))) {
            \Message::warning('Akce s takovým ID neexistuje');
            \Redirect::to('/admin/akce');
        }
        $documents = array_filter(explode(',', $akce["a_dokumenty"]));

        $akce = [
            'id' => $akce['a_id'],
            'jmeno' => $akce['a_jmeno'],
            'kde' => $akce['a_kde'],
            'datum' => \Format::date($akce['a_od'])
            . (($akce['a_od'] != $akce['a_do'])
               ? ' - ' . \Format::date($akce['a_do']) : ''),
            'kapacita' => $akce['a_kapacita'],
            'volno' => $akce['a_kapacita'] - count(\DBAkce::getAkceItems($id)),
            'showForm' => \Permissions::check('akce', P_MEMBER) && !$akce['a_lock'],
            'canEdit' => \Permissions::check('akce', P_OWNED)
        ];

        $documents = array_map(
            fn($item) => [
                'name' => $item['d_name'],
                'category' => \Settings::$documentTypes[$item['d_kategorie']],
                'removeButton' => (new \SubmitHelper('Odstranit'))->data('remove', $item['d_id'])
            ],
            \DBDokumenty::getMultipleById($documents)
        );

        $allDocuments = [];
        foreach ([2, 3, 0] as $category) {
            foreach (\DBDokumenty::getDokumentyByKategorie($category) as $item) {
                $allDocuments[$item['d_id']] =
                    \Settings::$documentTypes[$item['d_kategorie']] . ' - ' .
                    $item['d_name'];
            }
        }
        $documentSelect = new \SelectHelper('add-id', array_merge(['' => '---'], $allDocuments));
        $documents[] = [
            'name' => (string) $documentSelect,
            'category' => (new \SubmitHelper('Přidat'))->data('add', 'add'),
            'removeButton' => ''
        ];
        new \RenderHelper('files/View/Admin/Akce/Dokumenty.inc', [
            'header' => 'Správa akcí',
            'data' => $akce,
            'documents' => $documents
        ]);
    }

    public static function dokumentyPost($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        if (!($akce = \DBAkce::getSingleAkce($id))) {
            \Message::warning('Akce s takovým ID neexistuje');
            \Redirect::to('/admin/akce');
        }
        $documents = array_filter(explode(',', $akce["a_dokumenty"]));

        $changed = false;
        if (isset($_POST["remove"])) {
            unset($documents[array_search($_POST['remove'], $documents)]);
            $documents = array_values($documents);
            $changed = true;
        }
        if (isset($_POST["add-id"]) && \DBDokumenty::getSingleDokument($_POST["add-id"])) {
            $documents[] = $_POST["add-id"];
            unset($_POST['add-id']);
            $changed = true;
        }
        if ($changed) {
            \DBAkce::editAkce(
                $akce["a_id"],
                $akce["a_jmeno"],
                $akce["a_kde"],
                $akce["a_info"],
                $akce["a_od"],
                $akce["a_do"],
                $akce["a_kapacita"],
                implode(',', $documents),
                $akce["a_lock"],
                $akce['a_visible'],
            );
        }
        \Redirect::to('/admin/akce/dokumenty/' . $id);
    }

    private static function displayForm($action, $data = [])
    {
        if ($data) {
            $dokumenty = array_map(
                fn($item) => ['id' => $item['d_id'], 'name' => $item['d_name']],
                \DBDokumenty::getMultipleById(array_filter(explode(',', $data['a_dokumenty']))),
            );
        } else {
            $dokumenty = [];
        }

        new \RenderHelper('files/View/Admin/Akce/Form.inc', [
            'header' => 'Správa akcí',
            'subheader' => $action == 'add' ? 'Přidat akci' : 'Upravit akci',
            'dokumenty' => $dokumenty,
            'action' => $action == 'add' ? 'Přidat' : 'Upravit',
            'id' => $data ? $data['a_id'] : null,
            'jmeno' => $_POST['jmeno'] ?? $data['a_jmeno'] ?? '',
            'kde' => $_POST['kde'] ?? $data['a_kde'] ?? '',
            'info' => $_POST['info'] ?? $data['a_info'] ?? '',
            'od' => $_POST['od'] ?? $data['a_od'] ?? '',
            'do' => $_POST['do'] ?? $data['a_do'] ?? '',
            'kapacita' => $_POST['kapacita'] ?? $data['a_kapacita'] ?? '',
            'lock' => $_POST['lock'] ?? $data['a_lock'] ?? '',
            'visible' => $_POST['visible'] ?? $data['a_visible'] ?? ''
        ]);
    }

    private static function checkData(): \Form
    {
        $od = new \Date($_POST['od'] ?? null);
        $do = new \Date($_POST['do'] ?? null);

        $form = new \Form();
        $form->checkDate((string) $od, 'Špatný formát data ("Od")', 'od');
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $form->checkDate((string) $do, 'Špatný formát data ("Do")', 'do');
        }
        $form->checkNumeric($_POST['kapacita'], 'Kapacita musí být zadána číselně', 'kapacita');

        return $form;
    }
}
