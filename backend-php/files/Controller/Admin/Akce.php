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
            "INSERT INTO akce" .
            " (a_jmeno,a_kde,a_info,a_od,a_do,a_kapacita,a_dokumenty,a_lock,a_visible)" .
            " VALUES ('?','?','?','?','?','?','?','?','?')",
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
        $data = \Database::querySingle("SELECT * FROM akce WHERE a_id='?'", $id);
        if (!$data) {
            \Message::warning('Akce s takovým ID neexistuje');
            \Redirect::to('/admin/akce');
        }
        return self::displayForm('edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM akce WHERE a_id='?'", $id);
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
            "UPDATE akce SET a_jmeno='?',a_kde='?',a_info='?',a_od='?',a_do='?'," .
            "a_kapacita='?',a_dokumenty='?',a_lock='?',a_visible='?' WHERE a_id='?'",
            $_POST['jmeno'],
            $_POST['kde'],
            $_POST['info'],
            (string) $od,
            (string) $do,
            $_POST['kapacita'],
            $data['a_dokumenty'],
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
        $item = \Database::querySingle("SELECT * FROM akce WHERE a_id='?'", $id);
        \Render::twig('RemovePrompt.twig', [
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
        \Database::query("DELETE FROM akce WHERE a_id='?'", $id);
        \Database::query("DELETE FROM attendee_user WHERE event_id='?'", $id);
        \Message::success('Akce odebrány');
        \Redirect::to('/admin/akce');
    }

    public static function detail($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM akce WHERE a_id='?'", $id);
        $items = \Database::queryArray(
            "SELECT * FROM attendee_user LEFT JOIN users ON user_id=u_id WHERE event_id='?' ORDER BY u_prijmeni",
            $id
        );
        if (!$data) {
            \Message::warning('Akce s takovým ID neexistuje');
            \Redirect::to('/admin/akce');
        }
        unset($data['a_info']);

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
        $data = \Database::querySingle("SELECT * FROM akce WHERE a_id='?'", $id);
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
                $data = \DBUser::getUserData($user);
                list($year) = explode('-', $data['u_narozeni']);
                \Database::query(
                    "UPDATE attendee_user SET user_id='?',birth_year='?' WHERE id='?'",
                    $user,
                    $year,
                    $item['id'],
                );
            }
        }

        if (is_numeric($_POST["add-user"]) && $_POST['add-user'] > 0) {
            $user = $_POST["add-user"];
            $data = \DBUser::getUserData($user);
            list($year) = explode('-', $data['u_narozeni']);

            \Database::query(
                "INSERT INTO attendee_user (event_id, user_id, birth_year) VALUES ('?','?','?')",
                $id,
                $user,
                $year,
            );
            $_POST['add-user'] = 0;
        }
        \Redirect::to('/admin/akce/detail/' . $id);
    }

    private static function displayForm($action, $data = [])
    {
        \Render::twig('Admin/AkceForm.twig', [
            'action' => $action,
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
        $form->checkDate((string) $od, 'Špatný formát data ("Od")');
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $form->checkDate((string) $do, 'Špatný formát data ("Do")');
        }
        $form->checkNumeric($_POST['kapacita'], 'Kapacita musí být zadána číselně');

        return $form;
    }
}
