<?php
class Controller_Member extends Controller_Abstract
{
    public function __construct() {
        Permissions::checkError('nastenka', P_VIEW);
    }

    public function view($id = null)  {
        if (isset($_SESSION['zaplaceno_text'])) {
            $this->redirect()->setMessage($_SESSION['zaplaceno_text']);
        }

        $data = array_map(
            function($item) {
                return array(
                    'id' => $item['no_id'],
                    'text' => $item['no_text'],
                    'user' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'timestamp' => formatTimestamp($item['no_timestamp'])
                );
            },
            DBNovinky::getLastNovinky(NOVINKY_COUNT)
        );

        $this->render(
            'files/View/Member/Home.inc',
            array(
                'data' => $data,
                'canEdit' => Permissions::check('novinky', P_OWNED)
            )
        );
    }

    public function navbar() {
        return parent::navbar() .
            new Navbar(
                include SETTINGS . '/menu/member.inner.php',
                false
            );
    }
}
