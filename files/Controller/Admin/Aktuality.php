<?php
require_once 'files/Controller/Admin.php';
class Controller_Admin_Aktuality extends Controller_Admin
{
    function __construct() {
        Permissions::checkError('aktuality', P_OWNED);
    }
    function view($id = null) {
        if (empty($_POST)) {
            $this->render("files/Admin/Aktuality/Display.inc");
            return;
        }
        switch(post("action")) {
            case 'edit':
                $aktuality = post('aktuality');
                if ($aktuality[0])
                    $this->redirect('/admin/aktuality/edit/' . $aktuality[0]);
                break;
            case 'foto':
                $aktuality = post('aktuality');
                if ($aktuality[0])
                    $this->redirect('/admin/aktuality/foto/' . $aktuality[0]);
                break;
            case 'remove':
                if (!is_array(post('aktuality')))
                    break;
                $url = '/admin/aktuality/remove?';
                foreach (post('aktuality') as $id)
                    $url .= '&u[]=' . $id;
                $this->redirect($url);
                break;
        }
        $this->render("files/Admin/Aktuality/Display.inc");
    }
    function add($id = null) {
        if (empty($_POST)) {
            $this->render('files/Admin/Aktuality/Form.inc');
            return;
        }
        $preview = trim(substr(strip_tags(post('text')), 0, AKTUALITY_PREVIEW));

        if (($foto_data = DBGalerie::getFotky(post('foto'))) &&
                $foto_data[0]['gf_id'])
            $f_id = $foto_data[0]['gf_id'];
        else
            $f_id = 0;

        $id = DBAktuality::addAktualita(User::getUserID(), post('kat'), post('jmeno'),
            post('text'), $preview, post('foto'), $f_id);

        $n = new Novinky(User::getUserID());
        switch(post('kat')) {
            case AKTUALITY_VIDEA:
                $n->video()->add('/aktuality/' . $id, post('jmeno'));
            case AKTUALITY_CLANKY:
                $n->clanek()->add('/aktuality/' . $id, post('jmeno'));
        }
        $this->redirect('/admin/aktuality', 'Článek přidán');
    }
    function edit($id = null) {
        if (!$id || !($data = DBAktuality::getSingleAktualita($id)))
            $this->redirect('/admin/aktuality', 'Článek s takovým ID neexistuje');

        Permissions::checkError('aktuality', P_OWNED, $data['at_kdo']);

        if (empty($_POST)) {
            post('kat', $data['at_kat']);
            post("jmeno", $data["at_jmeno"]);
            post("text", stripslashes($data["at_text"]));
            post('foto', $data['at_foto']);

            $this->render("files/Admin/Aktuality/Form.inc");
            return;
        }
        if (post('kat') != $data['at_kat'] || post('jmeno') != $data['at_jmeno'] ||
                post('text') != $data['at_text'] || post('foto') != $data['at_foto']) {
            $preview = trim(substr(strip_tags(post('text')), 0, AKTUALITY_PREVIEW));

            if (($foto_data = DBGalerie::getFotky(post('foto'))) &&
                    $foto_data[0]['gf_id'])
                $f_id = $foto_data[0]['gf_id'];
            else
                $f_id = 0;

            DBAktuality::editAktualita($id, post('kat'), post('jmeno'), post('text'),
                $preview, post('foto'), $f_id);
            $changed = true;
        }
        if (isset($changed) && $changed) {
            $n = new Novinky(User::getUserID());
            switch(post('kat')) {
                case AKTUALITY_VIDEA:
                    $n->video()->edit('/aktuality/' . $id, post('jmeno'));
                case AKTUALITY_CLANKY:
                    $n->clanek()->edit('/aktuality/' . $id, post('jmeno'));
            }
        }

        $this->redirect('/admin/aktuality', 'Článek změněn');
    }
    function remove($id = null) {
        if (empty($_POST) || post('action') !== 'confirm') {
            $this->render('files/Admin/Aktuality/DisplayRemove.inc');
            return;
        }
        if (!is_array(post('aktuality')))
            $this->redirect('/admin/aktuality');
        foreach (post('aktuality') as $id) {
            $data = DBAktuality::getSingleAktualita($id);

            if (Permissions::check('aktuality', P_OWNED, $data['at_kdo'])) {
                DBAktuality::removeAktualita($id);
                $n = new Novinky(User::getUserID());
                switch($data['at_kat']) {
                    case AKTUALITY_VIDEA:
                        $n->video()->remove($data['at_jmeno']);
                    case AKTUALITY_CLANKY:
                        $n->clanek()->remove($data['at_jmeno']);
                }
            } else {
                $error = true;
            }
        }
        if (isset($error) && $error)
            throw new AuthorizationException("Máte nedostatečnou autorizaci pro tuto akci!");

        $this->redirect('/admin/aktuality', 'Články odebrány');
    }
}
?>