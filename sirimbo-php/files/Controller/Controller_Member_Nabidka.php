<?php
class Controller_Member_Nabidka
{
    public function view($request)
    {
        \Permissions::checkError('nabidka', P_VIEW);
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
                                 && \Permissions::check('nabidka', P_MEMBER)
                                 && ($item['p_id'] === Session::getParID()
                                     || \Permissions::check('nabidka', P_OWNED, $data['n_trener']))),
                            'deleteTicket' => $item['p_id'] . '-' . $data['n_id']
                        ];
                    },
                    \DBNabidka::getNabidkaItem($data['n_id'])
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
                    'canEdit' => \Permissions::check('nabidka', P_OWNED, $data['n_trener']),
                    'hourMax' => $data['n_max_pocet_hod'],
                    'hourTotal' => $data['n_pocet_hod'],
                    'hourReserved' => $obsazeno,
                    'hourFree' => $data['n_pocet_hod'] - $obsazeno,
                    'canAdd' => !$data['n_lock'] && \Permissions::check('nabidka', P_MEMBER),
                    'items' => $items
                ];
            },
            array_filter(
                \DBNabidka::getNabidka(),
                function ($item) {
                    return $item['n_visible'];
                }
            )
        );

        if (empty($data)) {
            return new \RenderHelper('files/View/Empty.inc', [
                'header' => 'Nabídka tréninků',
                'notice' => 'Žádná nabídka k dispozici'
            ]);
        }

        new \RenderHelper('files/View/Member/Nabidka/Overview.inc', [
            'header' => 'Nabídka tréninků',
            'data' => $data
        ]);
    }

    private function checkData($request, $data): \Form
    {
        $f = new \Form();
        $f->checkBool(!$data['n_lock'], 'Tato nabídka je uzamčená', '');
        if ($request->post('hodiny')) {
            $f->checkNumeric($request->post('hodiny'), 'Špatný počet hodin', 'hodiny');
        }
        return $f;
    }

    private function processPost($request)
    {
        if (!$request->post()) {
            return;
        }
        $data = \DBNabidka::getSingleNabidka($request->post('id'));
        $form = $this->checkData($request, $data);
        if (!$form->isValid()) {
            return new \MessageHelper('warning', $form->getMessages());
        }
        if ($request->post('hodiny') > 0) {
            if (!Session::getZaplacenoPar()) {
                new \MessageHelper('danger', 'Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
            } elseif ($data['n_max_pocet_hod'] > 0
                && (\DBNabidka::getNabidkaLessons($request->post('id'), Session::getParID())
                   + $request->post('hodiny')) > $data['n_max_pocet_hod']
            ) {
                new \MessageHelper('danger', 'Maximální počet hodin na pár je ' . $data['n_max_pocet_hod'] . '!');
            } elseif (($data['n_pocet_hod'] - \DBNabidka::getNabidkaItemLessons($request->post('id'))) < $request->post('hodiny')) {
                new \MessageHelper('danger', 'Tolik volných hodin tu není');
            } else {
                \DBNabidka::addNabidkaItemLessons(Session::getParID(), $request->post('id'), $request->post('hodiny'));
                $request->post('hodiny', null);
            }
        } elseif ($request->post('un_id') !== null) {
            list($u_id, $n_id) = explode('-', $request->post('un_id'));

            if (!\DBNabidka::getNabidkaLessons($n_id, $u_id)) {
                new \MessageHelper('danger', 'Neplatný požadavek!');
            } elseif ($u_id != Session::getParID() && !\Permissions::check('nabidka', P_OWNED, $data['n_trener'])) {
                new \MessageHelper('danger', 'Nedostatečná oprávnění!');
            } else {
                \DBNabidka::removeNabidkaItem($n_id, $u_id);
            }
        }
    }
}
