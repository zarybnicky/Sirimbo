<?php
class Controller_Admin_Nabidka_Detail extends Controller_Abstract
{
    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('nabidka', P_OWNED);
    }

    public function view($request)
    {
        if (!$id = $request->getId()) {
            $this->redirect()->warning('Nabídka s takovým ID neexistuje');
            $this->redirect('/admin/nabidka');
        }
        if (!$data = DBNabidka::getSingleNabidka($id)) {
            $this->redirect()->warning('Nabídka s takovým ID neexistuje');
            $this->redirect('/admin/nabidka');
        }
        Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);

        $items = DBNabidka::getNabidkaItem($id);
        $obsazeno = DBNabidka::getNabidkaItemLessons($id);
        $users = DBPary::getPartners();

        if (!$request->post()) {
            $userSelect = $this->userSelect()
                               ->users($users)
                               ->type('par')
                               ->idVar('p_id');

            $items = array_map(
                function ($item) use ($userSelect) {
                    return [
                        'user' =>
                        (string) $userSelect->set($item['ni_partner'])
                                            ->name($item['ni_id'] . '-partner'),
                        'lessonCount' => (
                            $this->text(
                                $item['ni_id'] . '-hodiny',
                                $item['ni_pocet_hod']
                            )->size(1)
                            ->render()
                        ),
                        'removeButton' => $this->submit('Odstranit')
                                               ->name('remove')
                                               ->value($item['ni_id'])
                                               ->render()
                    ];
                },
                $items
            );
            $items[] = [
                'user' => (string) $userSelect->set(null)
                                              ->name('add_partner'),
                'lessonCount' => $this->text('add_hodiny', '')->size('1'),
                'removeButton' => $this->submit('Přidat')->render()
            ];

            $this->render('files/View/Admin/Nabidka/Detail.inc', [
                'header' => 'Správa nabídky',
                'nabidka' => [
                    'id' => $data['n_id'],
                    'fullName' => $data['u_jmeno'] . ' ' . $data['u_prijmeni'],
                    'datum' => (
                        formatDate($data['n_od'])
                        . ($data['n_od'] != $data['n_do']
                           ? ' - ' . formatDate($data['n_do'])
                           : '')
                    ),
                    'canEdit' => false,
                    'hourMax' => $data['n_max_pocet_hod'],
                    'hourTotal' => $data['n_pocet_hod'],
                    'hourReserved' => $obsazeno ?: '',
                    'hourFree' => $data['n_pocet_hod'] - $obsazeno
                ],
                'obsazeno' => $obsazeno,
                'users' => $users,
                'items' => $items,
                'backlink' => $request->getReferer()
            ]);
            return;
        }

        if ($request->post("remove") > 0) {
            DBNabidka::removeNabidkaItem(
                $id,
                $request->post($request->post("remove") . "-partner")
            );
            $items = DBNabidka::getNabidkaItem($id);
            $obsazeno = DBNabidka::getNabidkaItemLessons($id);
        }

        $maxLessons = $data['n_max_pocet_hodin'];

        foreach ($items as $item) {
            $partner = $item["ni_partner"];
            $partnerNew = $request->post($item["ni_id"] . "-partner");
            $count = $item['ni_pocet_hod'];
            $countNew = $request->post($item["ni_id"] . "-hodiny");

            if ($partner != $partnerNew || $count != $countNew) {
                if (0 < $maxLessons && $maxLessons < $countNew) {
                    $countNew = $maxLessons;
                }
                DBNabidka::editNabidkaItem(
                    $item["ni_id"],
                    $partnerNew,
                    $countNew
                );
            }
        }
        $items = DBNabidka::getNabidkaItem($id);
        $obsazeno = DBNabidka::getNabidkaItemLessons($id);

        if (is_numeric($request->post("add_hodiny")) &&
            is_numeric($request->post("add_partner")) &&
            $request->post('add_partner')
        ) {
            $partner = $request->post('add_partner');
            $count = $request->post("add_hodiny");
            $request->post('add_partner', null);
            $request->post('add_hodiny', null);

            if (0 < $maxLessons && $maxLessons < $count) {
                $count = $maxLessons;
            }

            DBNabidka::addNabidkaItemLessons(
                $request->post("add_partner"),
                $id,
                $count
            );

            $items = DBNabidka::getNabidkaItem($id);
            $obsazeno = DBNabidka::getNabidkaItemLessons($id);
        }

        //-----Dorovnávání skutečného a nastaveného počtu hodin-----//
        if ($obsazeno > $data["n_pocet_hod"]) {
            DBNabidka::editNabidka(
                $id,
                $data["n_trener"],
                $obsazeno,
                $data['n_max_pocet_hod'],
                $data["n_od"],
                $data["n_do"],
                $data['n_visible'],
                ($data["n_lock"]) ? 1 : 0
            );
            $data = DBNabidka::getSingleNabidka($id);
        }
        $this->redirect('/admin/nabidka/detail/' . $id);
    }
}
