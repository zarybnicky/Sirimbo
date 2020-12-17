<?php
namespace Olymp\Controller\Admin;

class Rozpis
{
    public static function list()
    {
        \Permissions::checkError('rozpis', P_OWNED);
        $data = \Permissions::check('rozpis', P_ADMIN)
            ? \DBRozpis::getRozpis(true)
            : \DBRozpis::getRozpisyByTrener(\Session::getUserID(), true);
        $data = array_map(
            fn($item) => [
                'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                'datum' => formatDate($item['r_datum']),
                'kde' => $item['r_kde'],
                'visible' => new \CheckboxHelper($item['r_id'], '1', $item['r_visible']),
                'buttons' => \Buttons::duplicate('/admin/rozpis/duplicate/' . $item['r_id'])
                . '&nbsp;' . \Buttons::delete('/admin/rozpis/remove/' . $item['r_id']),
                'links' => (
                    '<a href="/admin/rozpis/edit/' . $item['r_id'] . '">obecné</a>, ' .
                    '<a href="/admin/rozpis/detail/' . $item['r_id'] . '">tréninky</a>'
                )
            ],
            $data
        );
        new \RenderHelper('files/View/Admin/Rozpis/Overview.inc', [
            'header' => 'Správa rozpisů',
            'data' => $data
        ]);
    }

    public static function listPost()
    {
        \Permissions::checkError('rozpis', P_OWNED);
        $data = \Permissions::check('rozpis', P_ADMIN)
            ? \DBRozpis::getRozpis(true)
            : \DBRozpis::getRozpisyByTrener(\Session::getUserID(), true);

        foreach ($data as $item) {
            $id = $item['r_id'];
            if ((bool) $_POST[$id] == (bool) $item['r_visible']) {
                continue;
            }
            \DBRozpis::editRozpis(
                $id,
                $item['r_trener'],
                $item['r_kde'],
                $item['r_datum'],
                $_POST[$id] ? '1' : '0',
                $item['r_lock'] ? '1' : '0'
            );
        }
        new \RedirectHelper('/admin/rozpis');
    }

    public static function add()
    {
        \Permissions::checkError('rozpis', P_OWNED);
        return static::displayForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('rozpis', P_OWNED);
        $form = static::checkData();
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm('add');
        }
        \Permissions::checkError('rozpis', P_OWNED, $_POST['trener']);
        \DBRozpis::addRozpis(
            $_POST['trener'],
            $_POST['kde'],
            (string) new \Date($_POST['datum'] ?? null),
            $_POST['visible'] ? '1' : '0',
            $_POST['lock'] ? '1' : '0'
        );
        new \RedirectHelper('/admin/rozpis');
    }

    public static function edit($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        if (!$data = \DBRozpis::getSingleRozpis($id)) {
            new \MessageHelper('warning', 'Rozpis s takovým ID neexistuje');
            new \RedirectHelper('/admin/rozpis');
        }
        \Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);
        return static::displayForm('edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        if (!$data = \DBRozpis::getSingleRozpis($id)) {
            new \MessageHelper('warning', 'Rozpis s takovým ID neexistuje');
            new \RedirectHelper('/admin/rozpis');
        }
        \Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);
        $form = static::checkData();
        if (!$form->isValid()) {
            new \MessageHelper('warning', $form->getMessages());
            return static::displayForm('edit', $data);
        }
        \DBRozpis::editRozpis(
            $id,
            $_POST['trener'],
            $_POST['kde'],
            (string) new \Date($_POST['datum'] ?? null),
            $_POST['visible'] ? '1' : '0',
            $_POST['lock'] ? '1' : '0'
        );
        new \RedirectHelper('/admin/rozpis');
    }

    public static function duplicate($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        $data = \DBRozpis::getSingleRozpis($id);
        $items = \DBRozpis::getRozpisItem($id);
        $newId = \DBRozpis::addRozpis(
            $data['r_trener'],
            $data['r_kde'],
            $data['r_datum'],
            $data['r_visible'],
            $data['r_lock']
        );
        foreach ($items as $item) {
            \DBRozpis::addRozpisItem(
                $newId,
                $item['ri_partner'],
                $item['ri_od'],
                $item['ri_do'],
                $item['ri_lock']
            );
        }
        new \RedirectHelper('/admin/rozpis');
    }

    public static function remove($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        $trener = \DBRozpis::getRozpisTrener($id);
        if (!\Permissions::check('rozpis', P_OWNED, $trener['u_id'])) {
            throw new \AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
        }
        \DBRozpis::removeRozpis($id);
        new \RedirectHelper('/admin/rozpis');
    }

    protected static function displayForm($action, $data = null)
    {
        $isAdmin = \Permissions::check('rozpis', P_ADMIN);
        $treneri = $isAdmin
                 ? \DBUser::getUsersByPermission('rozpis', P_OWNED)
                 : [\DBUser::getUserData(\Session::getUserID())];

        new \RenderHelper('files/View/Admin/Rozpis/Form.inc', [
            'header' => 'Správa rozpisů',
            'subheader' => ($data === null ? 'Přidat' : 'Upravit') . ' rozpis',
            'action' => $action,
            'treneri' => $treneri,
            'trener' => $_POST['trener'] ?? ($data ? $data['r_trener'] : ''),
            'kde' => $_POST['kde'] ?? ($data ? $data['r_kde'] : ''),
            'datum' => $_POST['datum'] ?? ($data ? $data['r_datum'] : ''),
            'visible' => $_POST['visible'] ?? ($data ? $data['r_visible'] : ''),
            'lock' => $_POST['lock'] ?? ($data ? $data['r_lock'] : '')
        ]);
    }

    private static function checkData(): \Form
    {
        $datum = new \Date($_POST['datum'] ?? null);

        $f = new \Form();
        $f->checkNumeric($_POST['trener'], 'Neplatný trenér', 'trener');
        $f->checkDate((string) $datum, 'Neplatný formát data', 'datum');
        return $f;
    }
}
