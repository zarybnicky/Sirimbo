<?php
namespace Olymp\Controller\Admin;

class Akce
{
    public static function list()
    {
        \Permissions::checkError('akce', P_OWNED);
        \Render::twig('Admin/Akce.twig');
    }

    public static function add()
    {
        \Permissions::checkError('akce', P_OWNED);
        return self::displayForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('akce', P_OWNED);
        $form = self::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::displayForm('add');
        }

        $od = new \Date($_POST['od'] ?? null);
        $do = new \Date($_POST['do'] ?? null);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }
        \Database::query(
            "INSERT INTO event (name,location_text,description,since,until,capacity,is_locked,is_visible) VALUES ('?','?','?','?','?','?','?','?')",
            $_POST['jmeno'],
            $_POST['kde'],
            $_POST['info'],
            (string) $od,
            (string) $do,
            $_POST['kapacita'],
            ($_POST['lock'] == 'lock') ? 1 : 0,
            $_POST['visible'] ? '1' : '0'
        );

        \Message::success('Akce přidána');
        \Redirect::to('/admin/akce');
    }

    public static function edit($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM event WHERE id='?'", $id);
        if (!$data) {
            \Message::warning('Akce s takovým ID neexistuje');
            \Redirect::to('/admin/akce');
        }
        return self::displayForm('edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM event WHERE id='?'", $id);
        if (!$data) {
            \Message::warning('Akce s takovým ID neexistuje');
            \Redirect::to('/admin/akce');
        }

        $form = self::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::displayForm('edit', $data);
        }

        $od = new \Date($_POST['od'] ?? null);
        $do = new \Date($_POST['do'] ?? null);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }

        \Database::query(
            "UPDATE event SET name='?', location_text='?', description='?', since='?', until='?', capacity='?', is_locked='?', is_visible='?' WHERE id='?'",
            $_POST['jmeno'],
            $_POST['kde'],
            $_POST['info'],
            (string) $od,
            (string) $do,
            $_POST['kapacita'],
            (($_POST['lock'] ?? '') == 'lock') ? 1 : 0,
            ($_POST['visible'] ?? '') ? '1' : '0',
            $id
        );

        \Message::success('Akce upravena');
        \Redirect::to('/admin/akce');
    }

    public static function remove($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        $item = \Database::querySingle("SELECT * FROM event WHERE id='?'", $id);
        \Render::twig('RemovePrompt.twig', [
            'header' => 'Správa akcí',
            'prompt' => 'Opravdu chcete odstranit akce:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/akce',
            'data' => [[
                'id' => $item['id'],
                'text' => $item['nane']
            ]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        \Database::query("DELETE FROM event WHERE id='?'", $id);
        \Database::query("DELETE FROM attendee_user WHERE event_id='?'", $id);
        \Message::success('Akce odebrány');
        \Redirect::to('/admin/akce');
    }

    public static function detail($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM event WHERE id='?'", $id);
        $items = \Database::queryArray("SELECT * FROM attendee_user LEFT JOIN users ON user_id=u_id WHERE event_id='?' ORDER BY u_prijmeni", $id);
        if (!$data) {
            \Message::warning('Akce s takovým ID neexistuje');
            \Redirect::to('/admin/akce');
        }
        unset($data['description']);

        \Render::twig('Admin/AkceDetail.twig', [
            'data' => $data + [
                'reserved' => count($items),
                'canEdit' => \Permissions::check('akce', P_OWNED, $id),
            ],
            'users' => \DBUser::getActiveUsers(),
            'items' => $items,
        ]);
    }

    public static function detailPost($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM event WHERE id='?'", $id);
        if (!$data) {
            \Message::warning('Akce s takovým ID neexistuje');
            \Redirect::to('/admin/akce');
        }
        if (isset($_POST["remove"])) {
            \Database::query("DELETE FROM attendee_user WHERE id='?'", $_POST['remove']);
        }

        $items = \Database::queryArray(
            "SELECT * FROM attendee_user LEFT JOIN users ON user_id=u_id WHERE event_id='?' ORDER BY u_prijmeni",
            $id
        );
        foreach ($items as $item) {
            $user = $_POST[$item["id"] . '-user'];

            if (!$user) {
                \Database::query("DELETE FROM attendee_user WHERE id='?'", $item['id']);
            } elseif ($user != $item["user_id"]) {
                \Database::query("UPDATE attendee_user SET user_id='?' WHERE id='?'", $user, $item['id']);
            }
        }

        if (is_numeric($_POST["add-user"]) && $_POST['add-user'] > 0) {
            \Database::query("INSERT INTO attendee_user (event_id, user_id) VALUES ('?', '?')", $id, $_POST["add-user"]);
        }
        \Redirect::to('/admin/akce/detail/' . $id);
    }

    private static function displayForm($action, $data = [])
    {
        \Render::twig('Admin/AkceForm.twig', [
            'action' => $action,
            'id' => $data ? $data['id'] : null,
            'jmeno' => $_POST['jmeno'] ?? $data['name'] ?? '',
            'kde' => $_POST['kde'] ?? $data['location_text'] ?? '',
            'info' => $_POST['info'] ?? $data['description'] ?? '',
            'od' => $_POST['od'] ?? $data['since'] ?? '',
            'do' => $_POST['do'] ?? $data['until'] ?? '',
            'kapacita' => $_POST['kapacita'] ?? $data['capacity'] ?? '',
            'lock' => $_POST['lock'] ?? $data['is_locked'] ?? '',
            'visible' => $_POST['visible'] ?? $data['is_visible'] ?? ''
        ]);
    }

    private static function checkData(): \Form
    {
        $od = new \Date($_POST['od'] ?? null);
        $do = new \Date($_POST['do'] ?? null);

        $form = new \Form();
        $form->checkDate((string) $od, 'Špatný formát data ("Od")');
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $form->checkDate((string) $do, 'Špatný formát data ("Do")');
        }
        $form->checkNumeric($_POST['kapacita'], 'Kapacita musí být zadána číselně');

        return $form;
    }
}
