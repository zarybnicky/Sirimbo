<?php
require_once 'files/Controller/Admin/Nabidka.php';
class Controller_Admin_Nabidka_Detail extends Controller_Admin_Nabidka
{
    public function __construct() {
        Permissions::checkError('nabidka', P_OWNED);
    }
    public function view($id = null) {
        if (!$id || !($data = DBNabidka::getSingleNabidka($id))) {
            $this->redirect(
                '/admin/nabidka',
                'Nabídka s takovým ID neexistuje'
            );
        }
        Permissions::checkError('nabidka', P_OWNED, $data['n_trener']);

        $items = DBNabidka::getNabidkaItem($id);
        $obsazeno = DBNabidka::getNabidkaItemLessons($id);
        $users = DBPary::getPartners();

        if (empty($_POST)) {
            $userSelect = $this->userSelect()
                               ->users($users)
                               ->type('par')
                               ->idVar('p_id');

            $items = array_map(
                function ($item) use ($userSelect) {
                    return array(
                        'user' =>
                        (string) $userSelect->defaultValue($item['ni_partner'])
                                            ->name($item['ni_id'] . '-partner'),
                        'lessonCount' => (
                            '<input type="text" name="' . $item['ni_id'] .
                            '-hodiny" value="' . $item['ni_pocet_hod'] .
                            '" size=1/>'
                        ),
                        'removeButton' => (string) $this->submit('Odstranit')
                                                        ->name('remove')
                                                        ->value($item['ni_id'])
                    );
                },
                $items
            );
            $items[] = array(
                'user' => (string) $userSelect->defaultValue(null)
                                              ->name('add_partner'),
                'lessonCount' => '<input type="text" name="add_hodiny" value="" size=1/>',
                'removeButton' => (string) $this->submit('Přidat')
            );
            
            $this->render(
                'files/View/Admin/Nabidka/Detail.inc',
                array(
                    'nabidka' => array(
                        'id' => $data['n_id'],
                        'fullName' => $data['u_jmeno'] . ' ' . $data['u_prijmeni'],
                        'datum' => formatDate($data['n_od'])
                        . ($data['n_od'] != $data['n_do'] ? ' - ' . formatDate($data['n_do']) : ''),
                        'canEdit' => false,
                        'hourMax' => $data['n_max_pocet_hod'],
                        'hourTotal' => $data['n_pocet_hod'],
                        'hourReserved' => $obsazeno,
                        'hourFree' => $data['n_pocet_hod'] - $obsazeno
                    ),
                    'obsazeno' => $obsazeno,
                    'users' => $users,
                    'items' => $items,
                    'backlink' => Request::getReferer()
                )
            );
            return;
        }

        if (post("remove") > 0) {
            DBNabidka::removeNabidkaItem($id, post(post("remove") . "-partner"));
            $items = DBNabidka::getNabidkaItem($id);
            $obsazeno = DBNabidka::getNabidkaItemLessons($id);
        }
        foreach ($items as $item) {
            if (post($item["ni_id"] . "-partner") != $item["ni_partner"]
                || post($item["ni_id"] . "-hodiny") != $item["ni_pocet_hod"]
            ) {
                if ($data['n_max_pocet_hod'] > 0 && post($item['ni_id'] . '-hodiny') > $data['n_max_pocet_hod']) {
                    post($item['ni_id'] . '-hodiny', $data['n_max_pocet_hod']);
                }

                $rozdil_hod = post($item["ni_id"] . "-hodiny") - $item["ni_pocet_hod"];
                if (($obsazeno + $rozdil_hod) > $data["n_pocet_hod"]) {
                    post("pocet_hod", $obsazeno + $rozdil_hod);
                }
                DBNabidka::editNabidkaItem($item["ni_id"], post($item["ni_id"] . "-partner"),
                    post($item["ni_id"] . "-hodiny"));
            }
        }
        $items = DBNabidka::getNabidkaItem($id);
        $obsazeno = DBNabidka::getNabidkaItemLessons($id);

        if (is_numeric(post("add_hodiny")) && is_numeric(post("add_partner")) && post('add_partner')) {
            $hodiny = post("add_hodiny");
            $partner = post("add_partner");

            if ($data['n_max_pocet_hod'] > 0 && post('add_hodiny') > $data['n_max_pocet_hod']) {
                post('add_hodiny', $data['n_max_pocet_hod']);
            }
            if (($obsazeno + post('add_hodiny')) > post("pocet_hod")) {
                post("pocet_hod", $obsazeno + post('add_hodiny'));
            }

            DBNabidka::addNabidkaItemLessons(post("add_partner"), $id,
                post("add_hodiny"));

            post('add_partner', null);
            post('add_hodiny', null);
            $items = DBNabidka::getNabidkaItem($id);
            $obsazeno = DBNabidka::getNabidkaItemLessons($id);
        }

        //-----Dorovnávání skutečného a nastaveného počtu hodin-----//
        if (post("pocet_hod") > $data["n_pocet_hod"]) {
            if (post("pocet_hod") < $obsazeno) {
                post("pocet_hod", $obsazeno);
            }
            DBNabidka::editNabidka(
                $id, $data["n_trener"], post("pocet_hod"), $data['n_max_pocet_hod'],
                $data["n_od"], $data["n_do"], $data['n_visible'], ($data["n_lock"]) ? 1 : 0
            );
            $data = DBNabidka::getSingleNabidka($id);
        }
        $this->redirect('/admin/nabidka/detail/' . $id);
  }
}
