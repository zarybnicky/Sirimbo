<?php
namespace TKOlomouc\Controller;

use TKOlomouc\Utility\Form;
use TKOlomouc\Utility\User;
use TKOlomouc\Model\DBUser;
use TKOlomouc\Utility\Response;

class Registrace extends ControllerAbstract
{
    public function view($id = null)
    {
        if (empty($_POST)) {
            $this->render('src/application/View/Main/Registrace.inc');
            return;
        }
        $narozeni = (new \TKOlomouc\View\Helper\Date('narozeni'))->getPost();

        $f = new Form();

        $f->checkLogin(post('username'), 'Špatný formát přihlašovacího jména', 'username');
        $f->checkPassword(post('pass'), 'Špatný formát hesla', 'pass');
        $f->checkLength(post('jmeno'), 1, 40, 'Špatná délka jména', 'jmeno');
        $f->checkLength(post('prijmeni'), 1, 40, 'Špatná délka přijmení', 'prijmeni');
        $f->checkInArray(post('pohlavi'), array('m', 'f'), 'Neplatné pohlaví', 'pohlavi');
        $f->checkEmail(post('email'), 'Neplatný formát emailu', 'email');
        $f->checkPhone(post('telefon'), 'Neplatný formát telefoního čísla', 'telefon');
        $f->checkDate((string) $narozeni, 'Neplatné datum narození', 'narozeni');

        if (!$f->isValid()) {
            Response::setMessage(implode('<br/>', $f->getMessages()));
            $this->render('src/application/View/Main/Registrace.inc');
            return;
        }

        $login = strtolower(post('username'));
        if (DBUser::getUserID($login)) {
            Response::setMessage('Už tu někdo s takovým přihlašovacím jménem je :o(');
            $this->render('src/application/View/Main/Registrace.inc');
            return;
        }
        User::register(
            $login, post('pass'), post('jmeno'), post('prijmeni'),
            post('pohlavi'), post('email'), post('telefon'),
            (string) $narozeni, post('poznamky')
        );
        $this->redirect(
            '/home',
            'Registrace úspěšně proběhla.<br /><br />'
                . 'Během několika dnů vám na email příjde potvrzení vašeho účtu,'
                . ' které vyřizuje administrátor ručně.'
        );
    }

    public function sidebar()
    {
    	;
    }
}
