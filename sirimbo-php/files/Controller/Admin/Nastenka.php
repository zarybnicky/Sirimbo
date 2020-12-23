<?php
namespace Olymp\Controller\Admin;

class Nastenka
{
    public static function list()
    {
        \Permissions::checkError('nastenka', P_OWNED);
        $pager = new \Paging(new \DBNastenka());
        $pager->setCurrentPage($_GET['p']);
        $pager->setItemsPerPage($_GET['c']);
        $data = array_map(
            fn($item) => [
                'buttons' => \Permissions::check('nastenka', P_OWNED, $item['up_kdo'])
                ? \Buttons::nastenka($item['up_id'])
                : '',
                'header' => $item['up_nadpis'],
                'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                'timestampAdd' => \Format::timestamp($item['up_timestamp_add'], true),
                'groups' => implode('', array_map(
                    fn($item) => new \ColorboxHelper($item['ups_color'], $item['ups_popis']),
                    \DBNastenka::getNastenkaSkupiny($item['up_id']),
                )),
            ],
            $pager->getItems(),
        );
        \Render::page('files/View/Admin/Nastenka/Overview.inc', [
            'header' => 'Správa nástěnky',
            'showButtonsCol' => !!array_filter(array_map(fn($x) => $x['buttons'], $data)),
            'data' => $data,
            'navigation' => $pager->getNavigation()
        ]);
    }

    public static function add()
    {
        \Permissions::checkError('nastenka', P_OWNED);
        return static::renderForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('nastenka', P_OWNED);
        $form = static::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::renderForm('add');
        }

        $id = \DBNastenka::addNastenka(
            \Session::getUser()->getId(),
            $_POST['nadpis'],
            $_POST['text'],
            $_POST['lock'] ? 1 : 0
        );

        $skupiny = \DBSkupiny::get();
        foreach ($skupiny as $skupina) {
            if (!$_POST['sk-' . $skupina['s_id']]) {
                continue;
            }
            \DBNastenka::addNastenkaSkupina(
                $id,
                $skupina['s_id'],
                $skupina['s_color_rgb'],
                $skupina['s_description']
            );
        }

        \Redirect::to($_POST['returnURI']);
    }

    public static function edit($id)
    {
        \Permissions::checkError('nastenka', P_OWNED);
        if (!$data = \DBNastenka::getSingleNastenka($id)) {
            \Message::warning('Nástěnka s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/nastenka');
        }
        \Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);
        $_POST['id'] = $id;
        $_POST['nadpis'] = $data['up_nadpis'];
        $_POST['text'] = $data['up_text'];
        foreach (\DBNastenka::getNastenkaSkupiny($id) as $skupina) {
            $_POST['sk-' . $skupina['ups_id_skupina']] = 1;
        }
        $_POST['lock'] = $data['up_lock'];
        return static::renderForm('edit');
    }

    public static function editPost($id)
    {
        \Permissions::checkError('nastenka', P_OWNED);
        if (!$data = \DBNastenka::getSingleNastenka($id)) {
            \Message::warning('Nástěnka s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/nastenka');
        }
        \Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);
        $form = static::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::renderForm('edit');
        }

        $skupiny_old = [];
        foreach (\DBNastenka::getNastenkaSkupiny($id) as $skupina) {
            $skupiny_old[$skupina['ups_id_skupina']] = $skupina['ups_id'];
        }
        $skupiny_new = [];
        foreach (\DBSkupiny::get() as $item) {
            if ($_POST['sk-' . $item['s_id']] ?? null) {
                $skupiny_new[$item['s_id']] = $item;
            }
        }

        $oldIds = array_keys($skupiny_old);
        $newIds = array_keys($skupiny_new);
        foreach (array_diff($oldIds, $newIds) as $removed) {
            \DBNastenka::removeNastenkaSkupina($skupiny_old[$removed]);
        }
        foreach (array_diff($newIds, $oldIds) as $added) {
            $skupinaData = $skupiny_new[$added];
            \DBNastenka::addNastenkaSkupina(
                $id,
                $skupinaData['s_id'],
                $skupinaData['s_color_rgb'],
                $skupinaData['s_description']
            );
        }
        \DBNastenka::editNastenka($id, $_POST['nadpis'], $_POST['text'], ($_POST['lock'] == 'lock') ? 1 : 0);
        \Redirect::to($_POST['returnURI']);
    }

    public static function remove($id)
    {
        \Permissions::checkError('nastenka', P_OWNED);
        if (!$data = \DBNastenka::getSingleNastenka($id)) {
            \Message::warning('Příspěvek s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/nastenka');
        }
        \Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);
        \Render::page('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa nástěnky',
            'prompt' => 'Opravdu chcete odstranit příspěvek:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/nastenka',
            'data' => [['id' => $data['up_id'], 'text' => $data['up_nadpis']]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('nastenka', P_OWNED);
        if (!$data = \DBNastenka::getSingleNastenka($id)) {
            \Message::warning('Příspěvek s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/nastenka');
        }
        \Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);
        \DBNastenka::removeNastenka($id);
        \Redirect::to('/admin/nastenka');
    }

    public static function renderForm($action)
    {
        $skupiny = \DBSkupiny::get();
        $skupinySelected = [];
        foreach ($skupiny as $item) {
            $skupinySelected[$item['s_id']] = $_POST['sk-' . $item['s_id']] ?? null;
        }
        \Render::page('files/View/Admin/Nastenka/Form.inc', [
            'header' => 'Správa nástěnky',
            'subheader' => ($action === 'add') ? 'Přidat příspěvek' : 'Upravit příspěvek',
            'action' => $action,
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/nastenka',
            'skupiny' => $skupiny,
            'skupinySelected' => $skupinySelected,
            'nadpis' => $_POST['nadpis'] ?? '',
            'text' => $_POST['text'] ?? '',
            'lock' => $_POST['lock'] ?? ''
        ]);
    }

    private static function checkData(): \Form
    {
        $f = new \Form();
        $f->checkNotEmpty($_POST['nadpis'], 'Zadejte nadpis', 'nadpis');
        $f->checkNotEmpty($_POST['text'], 'Zadejte nějaký text', 'text');
        return $f;
    }
}
