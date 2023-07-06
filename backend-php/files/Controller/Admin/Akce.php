<?php
namespace Olymp\Controller\Admin;

class Akce
{
    public static function list()
    {
        \Permissions::checkError('akce', P_OWNED);
        \Render::twig('Admin/Akce.twig', ['data' => \DBAkce::getWithItemCount()]);
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
        return self::displayForm('edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        if (!($data = \DBAkce::getSingleAkce($id))) {
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

        \DBAkce::editAkce(
            $id,
            $_POST['jmeno'],
            $_POST['kde'],
            $_POST['info'],
            (string) $od,
            (string) $do,
            $_POST['kapacita'],
            $data['a_dokumenty'],
            (($_POST['lock'] ?? '') == 'lock') ? 1 : 0,
            ($_POST['visible'] ?? '') ? '1' : '0'
        );

        \Message::success('Akce upravena');
        \Redirect::to('/admin/akce');
    }

    public static function remove($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        $item = \DBAkce::getSingleAkce($id);
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
        \DBAkce::removeAkce($id);
        \Message::success('Akce odebrány');
        \Redirect::to('/admin/akce');
    }

    public static function detail($id)
    {
        \Permissions::checkError('akce', P_OWNED);
        if (!$data = \DBAkce::getSingleAkce($id)) {
            \Message::warning('Akce s takovým ID neexistuje');
            \Redirect::to('/admin/akce');
        }
        unset($data['a_info']);

        \Render::twig('Admin/AkceDetail.twig', [
            'data' => $data + [
                'reserved' => count(\DBAkce::getAkceItems($id)),
                'canEdit' => \Permissions::check('akce', P_OWNED, $id),
            ],
            'users' => \DBUser::getActiveUsers(),
            'items' => \DBAkce::getAkceItems($id),
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

    private static function displayForm($action, $data = [])
    {
        \Render::twig('Admin/AkceForm.twig', [
            'dokumenty' => $data ? \DBDokumenty::getMultipleById(explode(',', $data['a_dokumenty'])) : [],
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
