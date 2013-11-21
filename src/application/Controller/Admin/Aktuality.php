<?php
namespace TKOlomouc\Controller\Admin;

use TKOlomouc\Controller\Admin;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\User;
use TKOlomouc\Utility\Novinky;
use TKOlomouc\Model\DBAktuality;
use TKOlomouc\Model\DBGalerie;
use TKOlomouc\View\Exception\AuthorizationException;
use TKOlomouc\Utility\Request;

class Aktuality extends Admin
{
    public function __construct()
    {
        Permissions::checkError('aktuality', P_OWNED);
    }

    public function view($id = null)
    {
        switch(post('action')) {
            case 'edit':
            case 'foto':
                $data = post('aktuality');
                if ($data[0]) {
                    $this->redirect('/admin/aktuality/' . post('action') . '/' . $data[0]);
                }
                break;
            case 'remove':
                if (is_array(post('akce'))) {
                    $this->redirect(
                        '/admin/aktuality/remove?' . http_build_query(
                            array('u' => post('aktuality'))
                        )
                    );
                }
                break;
        }
        $this->displayOverview();
    }

    public function add($id = null)
    {
        if (empty($_POST)) {
            $this->displayForm();
            return;
        }
        $preview = trim(substr(strip_tags(post('text')), 0, AKTUALITY_PREVIEW));

        if (($foto_data = DBGalerie::getFotky(post('foto')))
            && isset($foto_data[0]['gf_id'])
        ) {
            $f_id = $foto_data[0]['gf_id'];
        } else {
            $f_id = 0;
        }

        $id = DBAktuality::addAktualita(
            User::getUserID(),
            post('kat'),
            post('jmeno'),
            post('text'),
            $preview,
            post('foto'),
            $f_id
        );

        $news = new Novinky(User::getUserID());
        switch(post('kat')) {
            case AKTUALITY_VIDEA:
                $news->video()->add('/aktualne/' . $id, post('jmeno'));
                break;
            case AKTUALITY_CLANKY:
                $news->clanek()->add('/aktualne/' . $id, post('jmeno'));
                break;
        }
        $this->redirect('/admin/aktuality', 'Článek přidán');
    }

    public function edit($id = null)
    {
        if (!$id || !($data = DBAktuality::getSingleAktualita($id))) {
            $this->redirect('/admin/aktuality', 'Článek s takovým ID neexistuje');
        }
        Permissions::checkError('aktuality', P_OWNED, $data['at_kdo']);

        if (empty($_POST)) {
            post('kat', $data['at_kat']);
            post('jmeno', $data['at_jmeno']);
            post('text', stripslashes($data['at_text']));
            post('foto', $data['at_foto']);

            $this->displayForm();
            return;
        }

        $preview = trim(substr(strip_tags(post('text')), 0, AKTUALITY_PREVIEW));

        if (($foto_data = DBGalerie::getFotky(post('foto')))
            && isset($foto_data[0]['gf_id'])
        ) {
            $fotoId = $foto_data[0]['gf_id'];
        } else {
            $fotoId = 0;
        }

        DBAktuality::editAktualita(
            $id,
            post('kat'),
            post('jmeno'),
            post('text'),
            $preview,
            post('foto'),
            $fotoId
        );

        $news = new Novinky(User::getUserID());
        switch (post('kat')) {
            case AKTUALITY_VIDEA:
                $news->video()->edit('/aktualne/' . $id, post('jmeno'));
                break;
            case AKTUALITY_CLANKY:
                $news->clanek()->edit('/aktualne/' . $id, post('jmeno'));
                break;
        }
        $this->redirect('/admin/aktuality', 'Článek změněn');
    }

    public function remove($id = null)
    {
        if (!is_array(post('data')) && !is_array(get('u'))) {
            $this->redirect('/admin/aktuality');
        }
        if (!empty($_POST) && post('action') == 'confirm') {
            foreach (post('data') as $id) {
                $data = DBAktuality::getSingleAktualita($id);

                DBAktuality::removeAktualita($id);
                $news = new Novinky(User::getUserID());
                switch($data['at_kat']) {
                    case AKTUALITY_VIDEA:
                        $news->video()->remove($data['at_jmeno']);
                        break;
                    case AKTUALITY_CLANKY:
                        $news->clanek()->remove($data['at_jmeno']);
                        break;
                }
            }
            $this->redirect('/admin/aktuality', 'Články odebrány');
        }
        $data = array();
        foreach (get('u') as $id) {
            $item = DBAktuality::getSingleAktualita($id);
            $data[] = array(
                'id' => $item['at_id'],
                'text' => $item['at_jmeno']
            );
        }
        $this->render(
            'src/application/View/Admin/RemovePrompt.inc',
            array(
                'header' => 'Správa aktualit',
                'prompt' => 'Opravdu chcete odstranit články:',
                'returnURL' => Request::getReferer(),
                'data' => $data
            )
        );
    }

    private function displayForm()
    {
        $dirs = DBGalerie::getDirs(true, true);

        foreach ($dirs as &$item) {
            $newData = array(
                'id' => $item['gd_id'],
                'name' => str_repeat('-', $item['gd_level'] - 1) . ' ' . $item['gd_name']
            );
            $item = $newData;
        }

        $this->render(
            'src/application/View/Admin/Aktuality/Form.inc',
            array(
                'action' => Request::getAction(),
                'fotoDirs' => $dirs
            )
        );
    }

    private function displayOverview()
    {
        $data = DBAktuality::getAktuality();

        foreach ($data as &$item) {
            $newData = array(
                'id' => $item['at_id'],
                'name' => $item['at_jmeno'],
                'canEdit' => Permissions::check('aktuality', P_OWNED, $item['at_kdo'])
            );

            switch ($item['at_kat']) {
                case AKTUALITY_CLANKY:
                    $newData['type'] = 'Články';
                    break;
                case AKTUALITY_VIDEA:
                    $newData['type'] = 'Videa';
                    break;
                case AKTUALITY_KRATKE:
                    $newData['type'] = 'Krátné zprávy';
            }

            $item = $newData;
        }

        $this->render(
            'src/applocation/View/Admin/Aktuality/Display.inc',
            array(
                'data' => $data
            )
        );
    }
}
