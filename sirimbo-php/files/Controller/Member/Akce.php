<?php
namespace Olymp\Controller\Member;

class Akce
{
    public static function single($id)
    {
        \Permissions::checkError('akce', P_VIEW);
        if (!($data = \DBAkce::getSingleAkce($id, true))) {
            \Message::warning('Žádná taková akce neexistuje');
            \Redirect::to('/member/akce');
        }
        $data = static::_getRenderData($data);
        unset($data['a_info']);
        unset($data['dokumenty']);
        \Render::twig('Member/AkceSingle.twig', [
            'header' => 'Klubové akce',
            'data' => $data,
            'items' => \DBAkce::getAkceItems($id),
        ]);
    }

    public static function listPost()
    {
        \Permissions::checkError('akce', P_VIEW);
        if (!$_POST['id'] || !($data = \DBAkce::getSingleAkce($_POST['id']))) {
            \Message::warning('Žádná taková akce neexistuje');
            \Redirect::to('/member/akce');
        }

        $form = static::checkData($data, $_POST['action']);
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
        } elseif ($_POST['action'] == 'signup') {
            \DBAkce::signUp(\Session::getUser()->getId(), $_POST['id'], \Session::getUser()->getBirthYear());
        } elseif ($_POST['action'] == 'signout') {
            \DBAkce::signOut(\Session::getUser()->getId(), $_POST['id']);
        }
        \Redirect::to('/member/akce');
    }

    public static function list()
    {
        \Permissions::checkError('akce', P_VIEW);
        \Render::twig('Member/Akce.twig', [
            'header' => 'Klubové akce',
            'akce' => array_map(
                fn($item) => static::_getRenderData($item),
                \DBAkce::getAkce(true)
            ),
        ]);
    }

    private static function _getRenderData($data)
    {
        $items = \DBAkce::getAkceItems($data['a_id']);
        $showForm = \Permissions::check('akce', P_MEMBER) && !$data['a_lock'];
        $signedUp = \DBAkce::isUserSignedUp($data['a_id'], \Session::getUser()->getId());
        return $data + [
            'reserved' => count($items),
            'canEdit' => \Permissions::check('akce', P_OWNED, $data['a_id']),
            'signOut' => $showForm && $signedUp,
            'signIn' => $showForm && !$signedUp && $data['a_kapacita'] > count($items),
            'dokumenty' => \DBDokumenty::getMultipleById(explode(',', $data['a_dokumenty'])),
        ];
    }

    private static function checkData($data, $action): \Form
    {
        $f = new \Form();
        $f->checkBool(!$data['a_lock'], 'Tato akce je zamčená', '');
        $f->checkInArray($action, ['signup', 'signout'], 'Špatná akce', '');
        $f->checkNumeric($_POST['id'], 'Špatné ID', '');
        return $f;
    }
}
