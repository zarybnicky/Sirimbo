<?php
namespace Olymp\Controller\Admin;

class Nastenka
{
    public static function list()
    {
        \Permissions::checkError('nastenka', P_OWNED);
        \Render::twig('Admin/Nastenka.twig');
    }

    public static function add()
    {
        \Permissions::checkError('nastenka', P_OWNED);
        return self::renderForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('nastenka', P_OWNED);
        $form = self::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::renderForm('add');
        }

        \Database::query(
            "INSERT INTO upozorneni (up_nadpis,up_text) VALUES ('?','?')",
            $_POST['nadpis'],
            $_POST['text'],
        );
        $id = \Database::getInsertId();

        $skupiny = \DBSkupiny::get();
        foreach ($skupiny as $skupina) {
            if (!isset($_POST['sk-' . $skupina['s_id']]) || !$_POST['sk-' . $skupina['s_id']]) {
                continue;
            }
            \Database::query(
                "INSERT INTO upozorneni_skupiny (ups_id_rodic,ups_id_skupina,ups_color) VALUES ('?','?','?')",
                $skupina['s_id'],
                $skupina['s_color_rgb'],
                $id,
            );
        }

        \Redirect::to($_POST['returnURI']);
    }

    public static function edit($id)
    {
        \Permissions::checkError('nastenka', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM upozorneni WHERE up_id='?'", $id);
        $items = \Database::queryArray("SELECT * FROM upozorneni_skupiny WHERE ups_id_rodic='?'", $id);
        if (!$data) {
            \Message::warning('Nástěnka s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/nastenka');
        }
        \Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);
        $_POST['id'] = $id;
        $_POST['nadpis'] = $data['up_nadpis'];
        $_POST['text'] = $data['up_text'];
        foreach ($items as $skupina) {
            $_POST['sk-' . $skupina['ups_id_skupina']] = 1;
        }
        return self::renderForm('edit');
    }

    public static function editPost($id)
    {
        \Permissions::checkError('nastenka', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM upozorneni WHERE up_id='?'", $id);
        $items = \Database::queryArray("SELECT * FROM upozorneni_skupiny WHERE ups_id_rodic='?'", $id);
        if (!$data) {
            \Message::warning('Nástěnka s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/nastenka');
        }
        \Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);
        $form = self::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::renderForm('edit');
        }

        $skupiny_old = [];
        foreach ($items as $skupina) {
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
            \Database::query("DELETE FROM upozorneni_skupiny WHERE ups_id='?'", $skupiny_old[$removed]);
        }
        foreach (array_diff($newIds, $oldIds) as $added) {
            $skupinaData = $skupiny_new[$added];
            \Database::query(
                "INSERT INTO upozorneni_skupiny (ups_id_rodic,ups_id_skupina,ups_color) VALUES ('?','?','?')",
                $skupinaData['s_id'],
                $skupinaData['s_color_rgb'],
                $id,
            );
        }
        \Database::query(
            "UPDATE upozorneni SET up_nadpis='?',up_text='?' WHERE up_id='?'",
            $_POST['nadpis'],
            $_POST['text'],
            $id
        );
        \Redirect::to($_POST['returnURI']);
    }

    public static function remove($id)
    {
        \Permissions::checkError('nastenka', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM upozorneni WHERE up_id='?'", $id);
        if (!$data) {
            \Message::warning('Příspěvek s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/nastenka');
        }
        \Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);
        \Render::twig('RemovePrompt.twig', [
            'header' => 'Správa nástěnky',
            'prompt' => 'Opravdu chcete odstranit příspěvek:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/nastenka',
            'data' => [['id' => $data['up_id'], 'text' => $data['up_nadpis']]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('nastenka', P_OWNED);
        $data = \Database::querySingle("SELECT * FROM upozorneni WHERE up_id='?'", $id);
        if (!$data) {
            \Message::warning('Příspěvek s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/nastenka');
        }
        \Permissions::checkError('nastenka', P_OWNED, $data['up_kdo']);
        \Database::query("DELETE FROM upozorneni WHERE up_id='?'", $id);
        \Database::query("DELETE FROM upozorneni_skupiny WHERE ups_id_rodic='?'", $id);
        \Redirect::to('/admin/nastenka');
    }

    public static function renderForm($action)
    {
        $skupiny = \DBSkupiny::get();
        $skupinySelected = [];
        foreach ($skupiny as $item) {
            $skupinySelected[$item['s_id']] = $_POST['sk-' . $item['s_id']] ?? null;
        }
        \Render::twig('Admin/NastenkaForm.twig', [
            'action' => $action,
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/nastenka',
            'skupiny' => $skupiny,
            'skupinySelected' => $skupinySelected,
            'nadpis' => $_POST['nadpis'] ?? '',
            'text' => $_POST['text'] ?? '',
        ]);
    }

    private static function checkData(): \Form
    {
        $f = new \Form();
        $f->checkNotEmpty($_POST['nadpis'], 'Zadejte nadpis');
        $f->checkNotEmpty($_POST['text'], 'Zadejte nějaký text');
        return $f;
    }
}
