<?php
class Controller_Member_Nabidka extends Controller_Member
{
    public function __construct()
    {
        parent::__construct();
        Permissions::checkError('nabidka', P_VIEW);
    }

    public function view($request)
    {
        $this->processPost($request);

        $data = array_map(
            function ($data) {
                $items = array_map(
                    function ($item) use ($data) {
                        return [
                            'id' => $item['u_id'],
                            'fullName' => $item['u_jmeno'] . ' ' . $item['u_prijmeni'],
                            'hourCount' => $item['ni_pocet_hod'],
                            'canDelete' =>
                                (!$data['n_lock']
                                 && Permissions::check('nabidka', P_MEMBER)
                                 && ($item['p_id'] === User::getParID()
                                     || Permissions::check('nabidka', P_OWNED, $data['n_trener']))),
                            'deleteTicket' => $item['p_id'] . '-' . $data['n_id']
                        ];
                    },
                    DBNabidka::getNabidkaItem($data['n_id'])
                );

                $obsazeno = array_reduce(
                    $items,
                    function ($carry, $item) {
                        return $carry + $item['hourCount'];
                    },
                    0
                );

                return [
                    'id' => $data['n_id'],
                    'fullName' => $data['u_jmeno'] . ' ' . $data['u_prijmeni'],
                    'datum' => formatDate($data['n_od'])
                    . ($data['n_od'] != $data['n_do'] ? ' - ' . formatDate($data['n_do']) : ''),
                    'canEdit' => Permissions::check('nabidka', P_OWNED, $data['n_trener']),
                    'hourMax' => $data['n_max_pocet_hod'],
                    'hourTotal' => $data['n_pocet_hod'],
                    'hourReserved' => $obsazeno,
                    'hourFree' => $data['n_pocet_hod'] - $obsazeno,
                    'canAdd' => !$data['n_lock'] && Permissions::check('nabidka', P_MEMBER),
                    'items' => $items
                ];
            },
            array_filter(
                DBNabidka::getNabidka(),
                function ($item) {
                    return $item['n_visible'];
                }
            )
        );

        if (empty($data)) {
            $this->render('files/View/Empty.inc', [
                'header' => 'Nabídka tréninků',
                'notice' => 'Žádná nabídka k dispozici'
            ]);
            return;
        }

        $this->render('files/View/Member/Nabidka/Overview.inc', [
            'header' => 'Nabídka tréninků',
            'data' => $data
        ]);
    }

    private function checkData($request, $data)
    {
        $f = new Form();
        $f->checkBool(!$data['n_lock'], 'Tato nabídka je uzamčená', '');
        if ($request->post('hodiny')) {
            $f->checkNumeric($request->post('hodiny'), 'Špatný počet hodin', 'hodiny');
        }
        return $f->isValid() ? null : $f;
    }

    private function processPost($request)
    {
        if (!$request->post()) {
            return;
        }
        $data = DBNabidka::getSingleNabidka($request->post('id'));

        if (is_object($f = $this->checkData($request, $data))) {
            $this->redirect()->warning($f->getMessages());
            return;
        }
        if ($request->post('hodiny') > 0) {
            // if (!User::getZaplaceno(true)) {
            if (false) {
                $this->redirect()->danger('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
            } elseif ($data['n_max_pocet_hod'] > 0
                      && (DBNabidka::getNabidkaLessons($request->post('id'), User::getParID())
                          + $request->post('hodiny')) > $data['n_max_pocet_hod']
            ) {
                $this->redirect()->danger('Maximální počet hodin na pár je ' . $data['n_max_pocet_hod'] . '!');
            } elseif (($data['n_pocet_hod'] - DBNabidka::getNabidkaItemLessons($request->post('id'))) < $request->post('hodiny')) {
                $this->redirect()->danger('Tolik volných hodin tu není');
            } else {
                DBNabidka::addNabidkaItemLessons(User::getParID(), $request->post('id'), $request->post('hodiny'));
                $request->post('hodiny', null);
            }
        } elseif ($request->post('un_id') !== null) {
            list($u_id, $n_id) = explode('-', $request->post('un_id'));

            if (!DBNabidka::getNabidkaLessons($n_id, $u_id)) {
                $this->redirect()->danger('Neplatný požadavek!');
            } elseif ($u_id != User::getParID() && !Permissions::check('nabidka', P_OWNED, $data['n_trener'])) {
                $this->redirect()->danger('Nedostatečná oprávnění!');
            } else {
                DBNabidka::removeNabidkaItem($n_id, $u_id);
            }
        }
    }
}
