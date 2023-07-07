<?php
namespace Olymp\Controller\Admin;

class Skupiny
{
    public static function list()
    {
        \Permissions::checkError('skupiny', P_OWNED);
        \Render::twig('Admin/Skupiny.twig', ['data' => \DBSkupiny::get()]);
    }

    public static function add()
    {
        \Permissions::checkError('skupiny', P_OWNED);
        return self::displayForm(0, 'add');
    }

    public static function addPost()
    {
        \Permissions::checkError('skupiny', P_OWNED);
        $form = self::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::displayForm(0, 'add');
        }
        \Database::query(
            "INSERT INTO skupiny (s_name,s_location,s_color_text,s_color_rgb,s_description,s_visible)
            VALUES ('?','?','','?','?','?')",
            $_POST['name'],
            $_POST['location'],
            $_POST['color'],
            $_POST['desc'],
            ($_POST['visible'] ?? '') ? '1' : '0',
        );
        $insertId = \DBSkupiny::getInsertId();
        foreach ($_POST['group'] ?? [] as $item) {
            \DBSkupiny::addChild($insertId, $item);
        }
        \Redirect::to('/admin/skupiny');
    }

    public static function edit($id)
    {
        \Permissions::checkError('skupiny', P_OWNED);
        if (!$data = \DBSkupiny::getSingle($id)) {
            \Message::warning('Skupina s takovým ID neexistuje');
            \Redirect::to('/admin/skupiny');
        }
        return self::displayForm($id, 'edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('skupiny', P_OWNED);
        if (!$data = \DBSkupiny::getSingle($id)) {
            \Message::warning('Skupina s takovým ID neexistuje');
            \Redirect::to('/admin/skupiny');
        }
        $form = self::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::displayForm($id, 'edit', $data);
        }
        \Database::query(
            "UPDATE skupiny SET s_name='?',s_location='?',s_color_rgb='?',s_description='?',s_visible='?' WHERE s_id='?'",
            $_POST['name'],
            $_POST['location'],
            $_POST['color'],
            $_POST['desc'],
            ($_POST['visible'] ?? '') ? '1' : '0',
            $id,
        );

        $groupsOld = array_column(\DBSkupiny::getSingleWithGroups($id), 'pg_id');
        $groupsNew = $_POST['group'] ?? [];
        foreach (array_diff($groupsOld, $groupsNew) as $removed) {
            \DBSkupiny::removeChild($id, $removed);
        }
        foreach (array_diff($groupsNew, $groupsOld) as $added) {
            \DBSkupiny::addChild($id, $added);
        }
        \Redirect::to('/admin/skupiny');
    }

    public static function remove($id)
    {
        \Permissions::checkError('skupiny', P_OWNED);
        if (!$data = \DBSkupiny::getSingle($id)) {
            \Message::warning('Skupina s takovým ID neexistuje');
            \Redirect::to('/admin/skupiny');
        }
        if (self::getLinkedSkupinaObjects($id)) {
            \Message::info(
                'Nemůžu odstranit skupinu s připojenými kategoriemi! <form method="post">'
                . '<button class="btn btn-primary" name="action" value="unlink">Odstranit spojení?</button>'
                . '</form>'
            );
        }
        \Render::twig('RemovePrompt.twig', [
            'header' => 'Správa skupin',
            'prompt' => 'Opravdu chcete odstranit skupinu?',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/skupiny',
            'data' => [['id' => $data['s_id'], 'text' => $data['s_name']]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('skupiny', P_OWNED);
        if (!\DBSkupiny::getSingle($id)) {
            \Message::warning('Skupina s takovým ID neexistuje');
            \Redirect::to('/admin/skupiny');
        }
        if ($_POST['action'] == 'unlink') {
            $f = self::getLinkedSkupinaObjects($id);
            $groupCount = 0;
            foreach ($f['groups'] as $data) {
                \DBSkupiny::removeChild($id, $data['pg_id']);
                ++$groupCount;
            }
            \Message::info("Spojení s $groupCount kategoriemi byla odstraněna.");
            \Redirect::to('/admin/skupiny/remove/' . $id);
        }
        if (self::getLinkedSkupinaObjects($id)) {
            \Redirect::to('/admin/skupiny/remove/' . $id);
        }
        \Database::query("DELETE FROM skupiny WHERE s_id='?'", $id);
        \Redirect::to('/admin/skupiny');
    }

    private static function displayForm($id, $action, $data = [])
    {
        \Render::twig('Admin/SkupinyForm.twig', [
            'id' => $id,
            'name' => $_POST['name'] ?? $data['s_name'] ?? '',
            'location' => $_POST['location'] ?? $data['s_location'] ?? '',
            'color' => $_POST['color'] ?? $data['s_color_rgb'] ?? '',
            'popis' => $_POST['popis'] ?? $data['s_description'] ?? '',
            'visible' => $_POST['visible'] ?? $data['s_visible'] ?? '',
            'action' => $action,
            'groups' => \DBPlatbyGroup::getGroups(),
            'groupsSelected' => array_flip(array_column(\DBSkupiny::getSingleWithGroups($id), 'pg_id')),
        ]);
    }

    private static function getLinkedSkupinaObjects($id)
    {
        $group = \DBSkupiny::getSingleWithGroups($id);
        return $group ? ['groups' => $group] : [];
    }

    private static function checkData(): \Form
    {
        $f = new \Form();
        $f->checkNotEmpty($_POST['name'], 'Zadejte prosím nějaké jméno.');
        $f->checkNotEmpty($_POST['desc'], 'Zadejte prosím nějaký popis.');
        $f->checkRegexp($_POST['color'], '/#[0-9a-f]{6}/i', 'Zadejte prosím platnou barvu.');
        return $f;
    }
}
