<?php
namespace Olymp\Controller\Admin;

class Rozpis
{
    public static function add()
    {
        \Permissions::checkError('rozpis', P_OWNED);
        return static::displayForm();
    }

    public static function addPost()
    {
        \Permissions::checkError('rozpis', P_OWNED);

        $datum = new \Date($_POST['datum'] ?? null);

        $form = new \Form();
        $form->checkNumeric($_POST['trener'], 'Neplatný trenér');
        $form->checkDate((string) $datum, 'Neplatný formát data');
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm();
        }
        \Permissions::checkError('rozpis', P_OWNED, $_POST['trener']);
        \DBRozpis::addSchedule(
            $_POST['trener'],
            $_POST['kde'],
            (string) new \Date($_POST['datum'] ?? null),
            $_POST['visible'] ? '1' : '0',
            $_POST['lock'] ? '1' : '0'
        );
        \Redirect::to('/admin/rozpis');
    }

    public static function edit($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        if (!$data = \DBRozpis::getSchedule($id)) {
            \Message::warning('Rozpis s takovým ID neexistuje');
            \Redirect::to('/admin/rozpis');
        }
        \Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);
        return static::displayForm($data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        if (!$data = \DBRozpis::getSchedule($id)) {
            \Message::warning('Rozpis s takovým ID neexistuje');
            \Redirect::to('/admin/rozpis');
        }
        \Permissions::checkError('rozpis', P_OWNED, $data['r_trener']);

        \DBRozpis::editSchedule(
            $id,
            $_POST['trener'],
            $_POST['kde'],
            (string) new \Date($_POST['datum'] ?? null),
            $_POST['visible'] ? '1' : '0',
            $_POST['lock'] ? '1' : '0'
        );
        \Redirect::to('/admin/rozpis');
    }

    public static function duplicate($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        $data = \DBRozpis::getSchedule($id);
        $items = \DBRozpis::getLessons($id);
        $newId = \DBRozpis::addSchedule(
            $data['r_trener'],
            $data['r_kde'],
            $data['r_datum'],
            $data['r_visible'] ? '1' : '0',
            $data['r_lock'] ? '1' : '0'
        );
        foreach ($items as $item) {
            \DBRozpis::addLesson(
                $newId,
                $item['ri_partner'],
                $item['ri_od'],
                $item['ri_do'],
                $item['ri_lock'] ? '1' : '0'
            );
        }
        \Redirect::to('/admin/rozpis');
    }

    public static function remove($id)
    {
        \Permissions::checkError('rozpis', P_OWNED);
        $trener = \DBRozpis::getScheduleTrainer($id);
        if (!\Permissions::check('rozpis', P_OWNED, $trener['u_id'])) {
            throw new \AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
        }
        \DBRozpis::deleteSchedule($id);
        \Redirect::to('/admin/rozpis');
    }

    protected static function displayForm($data = null)
    {
        \Render::twig('Admin/RozpisForm.twig', [
            'action' => $data ? 'edit' : 'add',
            'treneri' => \Permissions::check('rozpis', P_ADMIN)
            ? \DBUser::getUsersByPermission('rozpis', P_OWNED)
            : [\DBUser::getUserData(\Session::getUser()->getId())],
            'trener' => $_POST['trener'] ?? ($data ? $data['r_trener'] : ''),
            'kde' => $_POST['kde'] ?? ($data ? $data['r_kde'] : ''),
            'datum' => $_POST['datum'] ?? ($data ? $data['r_datum'] : ''),
            'visible' => $_POST['visible'] ?? ($data ? $data['r_visible'] : ''),
            'lock' => $_POST['lock'] ?? ($data ? $data['r_lock'] : '')
        ]);
    }
}
