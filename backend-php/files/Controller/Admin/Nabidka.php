<?php
namespace Olymp\Controller\Admin;

class Nabidka
{
    public static function list()
    {
        \Permissions::checkError('nabidka', P_OWNED);
        \Render::twig('Admin/Nabidka.twig', [
            'data' => \Permissions::check('nabidka', P_ADMIN)
            ? \DBNabidka::getNabidka(true)
            : \DBNabidka::getNabidkyByTrener(\Session::getUser()->getId(), true),
        ]);
    }

    public static function add()
    {
        \Permissions::checkError('nabidka', P_OWNED);
        return self::displayForm('add');
    }

    public static function addPost()
    {
        \Permissions::checkError('nabidka', P_OWNED);
        $form = self::checkData();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return self::displayForm('add');
        }
        \Permissions::checkError('nabidka', P_OWNED, $_POST['trener']);

        $od = new \Date($_POST['od'] ?? null);
        $do = new \Date($_POST['do'] ?? null);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }
        if (!is_numeric($_POST['max_pocet_hod'])) {
            $_POST['max_pocet_hod'] = 0;
        }
        \DBNabidka::addNabidka(
            $_POST['trener'],
            $_POST['pocet_hod'],
            $_POST['max_pocet_hod'],
            (string) $od,
            (string) $do,
            $_POST['visible'] ? '1' : '0',
            $_POST['lock'] ? 1 : 0
        );
        \Message::success('Nabídka přidána');
        \Redirect::to($_POST['returnURI'] ?? '/admin/nabidka');
    }

    public static function edit($id)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        if (!$data = \DBNabidka::getSingleNabidka($id)) {
            \Message::warning('Nabídka s takovým ID neexistuje');
            \Redirect::to('/admin/nabidka');
        }
        \Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);
        return self::displayForm('edit', $data);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        if (!$data = \DBNabidka::getSingleNabidka($id)) {
            \Message::warning('Nabídka s takovým ID neexistuje');
            \Redirect::to($_POST['returnURI'] ?? '/admin/nabidka');
        }
        \Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);
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
        $items = \DBNabidka::getReservationLessons($id);
        $pocet_hod = $_POST['pocet_hod'];
        if ($pocet_hod < $items) {
            $pocet_hod = $items;
            \Message::warning('Obsazených hodin už je víc než jste zadali, nelze už dál snížit počet hodin');
        }
        $max_lessons = $_POST['max_pocet_hod'];
        $max_lessons_old = \DBNabidka::getNabidkaMaxItems($id);
        if ($max_lessons < $max_lessons_old && $max_lessons != 0) {
            $max_lessons = $max_lessons_old;
            \Message::warning(
                'Zadaný maximální počet hodin/pár je méně než už je zarezervováno, ' .
                'nelze už dál snížit maximální počet hodin'
            );
        }
        if (!is_numeric($max_lessons)) {
            $max_lessons = 0;
        }
        \DBNabidka::editNabidka(
            $id,
            $_POST['trener'],
            $pocet_hod,
            $max_lessons,
            (string) $od,
            (string) $do,
            $_POST['visible'] ? '1' : '0',
            $_POST['lock'] ? '1' : '0'
        );
        \Message::success('Nabídka úspěšně upravena');
        \Redirect::to($_POST['returnURI'] ?? '/admin/nabidka');
    }

    public static function duplicate($id)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        $data = \DBNabidka::getSingleNabidka($id);
        $items = \DBNabidka::getReservationItems($id);
        $newId = \DBNabidka::addNabidka(
            $data['n_trener'],
            $data['n_pocet_hod'],
            $data['n_max_pocet_hod'],
            $data['n_od'],
            $data['n_do'],
            $data['n_visible'] ? '1' : '0',
            $data['n_lock'] ? '1' : '0'
        );
        foreach ($items as $item) {
            \DBNabidka::addNabidkaItemLessons($item['ni_partner'], $newId, $item['ni_pocet_hod']);
        }
        \Redirect::to('/admin/nabidka');
    }

    public static function remove($id)
    {
        \Permissions::checkError('nabidka', P_OWNED);
        $data = \DBNabidka::getSingleNabidka($id);
        if (!\Permissions::check('nabidka', P_OWNED, $data['n_trener'])) {
            throw new \AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");
        }
        \DBNabidka::removeNabidka($id);
        \Redirect::to('/admin/nabidka');
    }

    protected static function displayForm($action, $data = [])
    {
        \Render::twig('Admin/NabidkaForm.twig', [
            'action' => $action,
            'returnURI' => $_SERVER['HTTP_REFERER'],
            'users' => \Permissions::check('nabidka', P_ADMIN)
            ? \DBUser::getUsersByPermission('nabidka', P_OWNED)
            : [\DBUser::getUserData(\Session::getUser()->getId())],
            'id' => $data['n_id'] ?? null,
            'trener' => $_POST['trener'] ?? $data['n_trener'] ?? '',
            'pocet_hod' => $_POST['pocet_hod'] ?? $data['n_pocet_hod'] ?? '',
            'max_pocet_hod' => $_POST['max_pocet_hod'] ?? $data['n_max_pocet_hod'] ?? '',
            'od' => $_POST['od'] ?? $data['n_od'] ?? '',
            'do' => $_POST['do'] ?? $data['n_do'] ?? '',
            'visible' => $_POST['visible'] ?? $data['n_visible'] ?? false,
            'lock' => $_POST['lock'] ?? $data['n_lock'] ?? ''
        ]);
    }

    private static function checkData(): \Form
    {
        $od = new \Date($_POST['od'] ?? null);
        $do = new \Date($_POST['do'] ?? null);
        if (!$do->isValid() || strcmp((string) $od, (string) $do) > 0) {
            $do = $od;
        }

        $f = new \Form();
        $f->checkNumeric($_POST['trener'], 'ID trenéra musí být číselné');
        $f->checkNumeric($_POST['pocet_hod'], 'Počet hodin prosím zadejte čísly');
        $f->checkDate((string) $od, 'Zadejte prosím platné datum ("Od")');
        if ($do->isValid()) {
            $f->checkDate((string) $do, 'Zadejte prosím platné datum ("Do")');
        }

        return $f;
    }
}
