<?php
namespace Olymp\Controller\Admin;

class Skupiny
{
    public static function add()
    {
        \Permissions::checkError('skupiny', P_OWNED);
        return static::displayForm(0, 'add');
    }

    public static function addPost()
    {
        \Permissions::checkError('skupiny', P_OWNED);
        $form = new \Form();
        $form->checkNotEmpty($_POST['name'], 'Zadejte prosím nějaké jméno.');
        $form->checkNotEmpty($_POST['desc'], 'Zadejte prosím nějaký popis.');
        $form->checkRegexp($_POST['color'], '/#[0-9a-f]{6}/i', 'Zadejte prosím platnou barvu.');
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm(0, 'add');
        }

        \DBSkupiny::insert(
            $_POST['name'],
            $_POST['location'],
            $_POST['color'],
            $_POST['desc'],
            ($_POST['visible'] ?? '') ? '1' : '0',
        );
        \Redirect::to('/admin/skupiny');
    }

    public static function edit($id)
    {
        \Permissions::checkError('skupiny', P_OWNED);
        if (!$data = \DBSkupiny::getSingle($id)) {
            \Message::warning('Skupina s takovým ID neexistuje');
            \Redirect::to('/admin/skupiny');
        }
        return static::displayForm($id, 'edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('skupiny', P_OWNED);
        if (!\DBSkupiny::getSingle($id)) {
            \Message::warning('Skupina s takovým ID neexistuje');
            \Redirect::to('/admin/skupiny');
        }

        \DBSkupiny::update(
            $id,
            $_POST['name'],
            $_POST['location'],
            $_POST['color'],
            $_POST['desc'],
            ($_POST['visible'] ?? '') ? '1' : '0',
        );
        \Redirect::to('/admin/skupiny');
    }

    public static function remove($id)
    {
        \Permissions::checkError('skupiny', P_OWNED);
        if (!$data = \DBSkupiny::getSingle($id)) {
            \Message::warning('Skupina s takovým ID neexistuje');
            \Redirect::to('/admin/skupiny');
        }
        if (static::getLinkedSkupinaObjects($id)) {
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
        if (static::getLinkedSkupinaObjects($id)) {
            \Redirect::to('/admin/skupiny/remove/' . $id);
        }
        \DBSkupiny::delete($id);
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
}
