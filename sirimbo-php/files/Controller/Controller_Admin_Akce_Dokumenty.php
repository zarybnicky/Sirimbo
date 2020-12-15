<?php
class Controller_Admin_Akce_Dokumenty
{
    public function view($request)
    {
        \Permissions::checkError('akce', P_OWNED);
        if (!$id = $request->getId()) {
            new \MessageHelper('warning', 'Akce s takovým ID neexistuje');
            new \RedirectHelper('/admin/akce');
        }
        if (!$akce = \DBAkce::getSingleAkce($id)) {
            new \MessageHelper('warning', 'Akce s takovým ID neexistuje');
            new \RedirectHelper('/admin/akce');
        }
        $documents = array_filter(explode(',', $akce["a_dokumenty"]));

        if ($_POST) {
            $changed = false;
            if ($_POST["remove"] !== null) {
                unset($documents[array_search($_POST['remove'], $documents)]);
                $documents = array_values($documents);
                $changed = true;
            }
            if ($_POST["add-id"] && \DBDokumenty::getSingleDokument($_POST["add-id"])) {
                $documents[] = $_POST["add-id"];
                $_POST['add-id'] = 0;
                $changed = true;
            }
            if ($changed) {
                \DBAkce::editAkce(
                    $akce["a_id"], $akce["a_jmeno"], $akce["a_kde"],
                    $akce["a_info"], $akce["a_od"], $akce["a_do"],
                    $akce["a_kapacita"], implode(',', $documents),
                    $akce["a_lock"], $akce['a_visible']
                );
            }
            new \RedirectHelper('/admin/akce/dokumenty/' . $id);
        }

        $booked = count(\DBAkce::getAkceItems($id));
        $akce = [
            'id' => $akce['a_id'],
            'jmeno' => $akce['a_jmeno'],
            'kde' => $akce['a_kde'],
            'datum' => formatDate($akce['a_od'])
            . (($akce['a_od'] != $akce['a_do'])
               ? ' - ' . formatDate($akce['a_do']) : ''),
            'kapacita' => $akce['a_kapacita'],
            'volno' => $akce['a_kapacita'] - $booked,
            'showForm' => \Permissions::check('akce', P_MEMBER) && !$akce['a_lock'],
            'canEdit' => \Permissions::check('akce', P_OWNED)
        ];

        $documents = array_map(fn($item) => [
            'name' => $item['d_name'],
            'category' => \Settings::$documentTypes[$item['d_kategorie']],
            'removeButton' => (new \SubmitHelper('Odstranit'))->data('remove', $item['d_id'])
        ], \DBDokumenty::getMultipleById($documents));

        $allDocuments = [];
        foreach ([2, 3, 0] as $category) {
            foreach (\DBDokumenty::getDokumentyByKategorie($category) as $item) {
                $allDocuments[$item['d_id']] =
                    \Settings::$documentTypes[$item['d_kategorie']] . ' - ' .
                    $item['d_name'];
            }
        }
        $documentSelect = new \SelectHelper('add-id', array_merge(['' => '---'], $allDocuments));
        $documents[] = [
            'name' => (string) $documentSelect,
            'category' => (new \SubmitHelper('Přidat'))->data('add', 'add'),
            'removeButton' => ''
        ];
        new \RenderHelper('files/View/Admin/Akce/Dokumenty.inc', [
            'header' => 'Správa akcí',
            'data' => $akce,
            'documents' => $documents
        ]);
    }
}
