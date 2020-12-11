<?php
class Controller_Admin_Akce_Dokumenty extends Controller_Abstract
{
    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('akce', P_OWNED);
    }

    public function view($request)
    {
        if (!$id = $request->getId()) {
            $this->redirect()->warning('Akce s takovým ID neexistuje');
            $this->redirect('/admin/akce');
        }
        if (!$akce = DBAkce::getSingleAkce($id)) {
            $this->redirect()->warning('Akce s takovým ID neexistuje');
            $this->redirect('/admin/akce');
        }
        $documents = array_filter(explode(',', $akce["a_dokumenty"]));

        if ($request->post()) {
            $changed = false;
            if ($request->post("remove") !== null) {
                unset($documents[array_search($request->post('remove'), $documents)]);
                $documents = array_values($documents);
                $changed = true;
            }
            if ($request->post("add-id") && DBDokumenty::getSingleDokument($request->post("add-id"))) {
                $documents[] = $request->post("add-id");
                $request->post('add-id', 0);
                $changed = true;
            }
            if ($changed) {
                DBAkce::editAkce(
                    $akce["a_id"], $akce["a_jmeno"], $akce["a_kde"],
                    $akce["a_info"], $akce["a_od"], $akce["a_do"],
                    $akce["a_kapacita"], implode(',', $documents),
                    $akce["a_lock"], $akce['a_visible']
                );
            }
            $this->redirect('/admin/akce/dokumenty/' . $id);
        }

        $booked = count(DBAkce::getAkceItems($id));
        $akce = [
            'id' => $akce['a_id'],
            'jmeno' => $akce['a_jmeno'],
            'kde' => $akce['a_kde'],
            'datum' => formatDate($akce['a_od'])
            . (($akce['a_od'] != $akce['a_do'])
               ? ' - ' . formatDate($akce['a_do']) : ''),
            'kapacita' => $akce['a_kapacita'],
            'volno' => $akce['a_kapacita'] - $booked,
            'showForm' => Permissions::check('akce', P_MEMBER) && !$akce['a_lock'],
            'canEdit' => Permissions::check('akce', P_OWNED)
        ];

        $documents = array_map(
            function($item) {
                return [
                    'name' => $item['d_name'],
                    'category' => Settings::$documentTypes[$item['d_kategorie']],
                    'removeButton' => $this->submit('Odstranit')->data('remove', $item['d_id'])
                ];
            },
            DBDokumenty::getMultipleById($documents)
        );

        $allDocuments = [];
        foreach ([2, 3, 0] as $category) {
            foreach (DBDokumenty::getDokumentyByKategorie($category) as $item) {
                $allDocuments[$item['d_id']] =
                    Settings::$documentTypes[$item['d_kategorie']] . ' - ' .
                    $item['d_name'];
            }
        }
        $documentSelect = $this->select()
                               ->name('add-id')
                               ->option('', '----------')
                               ->options($allDocuments);
        $documents[] = [
            'name' => (string) $documentSelect,
            'category' => $this->submit('Přidat')->data('add', 'add'),
            'removeButton' => ''
        ];
        $this->render('files/View/Admin/Akce/Dokumenty.inc', [
            'header' => 'Správa akcí',
            'data' => $akce,
            'documents' => $documents
        ]);
    }
}
