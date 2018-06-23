<?php
class Controller_Member extends Controller_Abstract
{
    public function __construct()
    {
        Permissions::checkError('nastenka', P_VIEW);
    }

    public function view($request)
    {
        if (isset($_SESSION['zaplaceno_text'])) {
            $this->redirect()->setMessage($_SESSION['zaplaceno_text']);
        }

        $data = array_map(
            function ($item) {
                return [
                    'id' => $item['no_id'],
                    'text' => $item['no_text'],
                    'user' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                    'timestamp' => formatTimestamp($item['no_timestamp'])
                ];
            },
            DBNovinky::getLastNovinky(10)
        );

        $this->render(
            'files/View/Member/Home.inc',
            [
                'data' => $data,
                'canEdit' => Permissions::check('novinky', P_OWNED)
            ]
        );
    }

    public function navbar()
    {
        return array_merge(parent::navbar(), [include SETTINGS . '/menu/member.inner.php']);
    }
}
