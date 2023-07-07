<?php
namespace Olymp\Controller\Member;

class Akce
{
    public static function single($id)
    {
        \Permissions::checkError('akce', P_VIEW);
        $data = \Database::querySingle("SELECT * FROM akce WHERE a_id='?' AND a_visible='1' ORDER BY a_od", $id);

        if (!$data) {
            \Message::warning('Žádná taková akce neexistuje');
            \Redirect::to('/member/akce');
        }
        $items = \Database::queryArray(
            "SELECT * FROM attendee_user LEFT JOIN users ON user_id=u_id WHERE event_id='?' ORDER BY u_prijmeni",
            $data['a_id']
        );
        $row = \Database::querySingle(
            "SELECT id FROM attendee_user WHERE event_id='?' AND user_id='?'",
            $data['a_id'],
            \Session::getUser()->getId()
        );
        $signedUp = $row ? (bool) $row["id"] : false;
        $showForm = \Permissions::check('akce', P_MEMBER) && !$data['a_lock'];
        unset($data['a_info']);
        \Render::twig('Member/AkceSingle.twig', [
            'data' => $data + [
                'reserved' => count($items),
                'canEdit' => \Permissions::check('akce', P_OWNED, $data['a_id']),
                'signOut' => $showForm && $signedUp,
                'signIn' => $showForm && !$signedUp && $data['a_kapacita'] > count($items)
            ],
            'items' => $items,
        ]);
    }

    public static function listPost()
    {
        \Permissions::checkError('akce', P_VIEW);
        $data = \Database::querySingle("SELECT * FROM akce WHERE a_id='?' AND a_visible='1' ORDER BY a_od", $_POST['id']);
        if (!$data) {
            \Message::warning('Žádná taková akce neexistuje');
            \Redirect::to('/member/akce');
        }

        $form = self::checkData($data, $_POST['action']);
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
        } elseif ($_POST['action'] == 'signup') {
            \Database::query(
                "INSERT INTO attendee_user (event_id,user_id,birth_year) VALUES ('?','?','?')",
                \Session::getUser()->getId(),
                $_POST['id'],
                \Session::getUser()->getBirthYear()
            );
        } elseif ($_POST['action'] == 'signout') {
            \Database::query("DELETE FROM attendee_user WHERE user_id='?' AND event_id='?'", \Session::getUser()->getId(), $_POST['id']);
        }
        \Redirect::to('/member/akce');
    }

    public static function list()
    {
        \Permissions::checkError('akce', P_VIEW);
        $data = \Database::queryArray("SELECT * FROM akce WHERE a_visible='1' ORDER BY a_do DESC, a_od DESC");
        \Render::twig('Member/Akce.twig', [
            'akce' => array_for($data, function ($data) {
                $items = \Database::queryArray(
                    "SELECT * FROM attendee_user LEFT JOIN users ON user_id=u_id WHERE event_id='?' ORDER BY u_prijmeni",
                    $data['a_id']
                );
                $showForm = \Permissions::check('akce', P_MEMBER) && !$data['a_lock'];
                $row = \Database::querySingle(
                    "SELECT id FROM attendee_user WHERE event_id='?' AND user_id='?'",
                    $data['a_id'],
                    \Session::getUser()->getId()
                );
                $signedUp = $row ? (bool) $row["id"] : false;
                return $data + [
                    'reserved' => count($items),
                    'canEdit' => \Permissions::check('akce', P_OWNED, $data['a_id']),
                    'signOut' => $showForm && $signedUp,
                    'signIn' => $showForm && !$signedUp && $data['a_kapacita'] > count($items)
                ];
            }),
        ]);
    }

    private static function checkData($data, $action): \Form
    {
        $f = new \Form();
        $f->checkBool(!$data['a_lock'], 'Tato akce je zamčená');
        $f->checkInArray($action, ['signup', 'signout'], 'Špatná akce');
        $f->checkNumeric($_POST['id'], 'Špatné ID');
        return $f;
    }
}
