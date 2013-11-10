<?php
require_once 'files/Controller/Member.php';
class Controller_Member_Rozpis  extends Controller_Member
{
    function __construct() {
        Permissions::checkError('rozpis', P_VIEW);
    }
    function view($id = null) {
        if (empty($_POST)) {
            $this->render('files/Member/Rozpis.inc');
            return;
        }
        $data = DBRozpis::getSingleRozpis(post('ri_id'));
        $lesson = DBRozpis::getRozpisItemLesson(post('ri_id'));

        if (post('action') == 'signup' && !is_object($this->_checkData($data, post('action')))) {
            if (!User::getZaplaceno() || (User::getPartnerID() > 0 && !User::getZaplaceno(true))) {
                $this->redirect()->setMessage('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
            } elseif ($lesson['ri_partner']) {
                $this->redirect()->setMessage('Už je obsazeno');
            } else {
                DBRozpis::rozpisSignUp(post('ri_id'), User::getParID());
                $this->redirect()->setMessage('Hodina přidána');
            }
        } elseif (post('action') == 'signout' && !is_object($this->_checkData($data, post('action')))) {
            if ($lesson['ri_partner'] == 0) {
                $this->redirect()->setMessage('Už je prázdno');
            } elseif (User::getParID() != $lesson['ri_partner'] && !Permissions::check('rozpis', P_OWNED, $data['n_trener'])) {
                $this->redirect()->setMessage('Nedostatečná oprávnění!');
            } else {
                DBRozpis::rozpisSignOut(post('ri_id'));
                $this->redirect()->setMessage('Hodina odebrána');
            }
        }
        $this->render('files/Member/Rozpis.inc');
    }
    private function _checkData($data, $action = 'signup') {
        $f = new Form();
        $f->checkBool(!$data['r_lock'], 'Tento rozpis je uzamčený', '');
        $f->checkInArray($action, array('signup', 'signout'), 'Špatná akce', '');

        return $f->isValid() ? true : $f;
    }
}
?>