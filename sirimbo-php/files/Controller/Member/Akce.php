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
        \Render::page('files/View/Member/Akce/Single.inc', [
            'header' => 'Klubové akce',
            'data' => static::_getRenderData($data)
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
        $data = \DBAkce::getAkce(true);
        if (!$data) {
            \Render::page('files/View/Empty.inc', [
                'header' => 'Klubové akce',
                'notice' => 'Žádné akce nejsou k dispozici.'
            ]);
        } else {
            \Render::page('files/View/Member/Akce/Overview.inc', [
                'header' => 'Klubové akce',
                'akce' => array_map(fn($item) => static::_getRenderData($item), $data),
            ]);
        }
    }

    private static function _getRenderData($data)
    {
        $items = \DBAkce::getAkceItems($data['a_id']);
        $dokumenty = array_map(
            fn($item) => ['id' => $item, 'name' => \DBDokumenty::getSingleDokument($item)['d_name']],
            array_filter(explode(',', $data['a_dokumenty'])) ?? []
        );

        $out = [
            'id' => $data['a_id'],
            'jmeno' => $data['a_jmeno'],
            'kde' => $data['a_kde'],
            'datum' => \Format::date($data['a_od']) .
                (($data['a_od'] != $data['a_do']) ? ' - ' . \Format::date($data['a_do']) : ''),
            'kapacita' => $data['a_kapacita'],
            'volno' => $data['a_kapacita'] - count($items),
            'showForm' => \Permissions::check('akce', P_MEMBER) && !$data['a_lock'],
            'canEdit' => \Permissions::check('akce', P_OWNED),
            'info' => nl2br($data['a_info']),
            'dokumenty' => $dokumenty,
            'items' => $items
        ];
        $out['signOut'] = $out['showForm'] && \DBAkce::isUserSignedUp($out['id'], \Session::getUser()->getId());
        $out['signIn'] = $out['showForm'] && !$out['signOut'] && $out['volno'] > 0;
        return $out;
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
