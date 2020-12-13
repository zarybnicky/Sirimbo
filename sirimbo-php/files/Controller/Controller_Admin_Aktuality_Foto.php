<?php
class Controller_Admin_Aktuality_Foto extends Controller_Abstract
{
    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('aktuality', P_OWNED);
    }

    public function view($request)
    {
        $id = $request->getId();
        if (!($article = DBAktuality::getSingleAktualita($id))) {
            $this->redirect()->warning('Takový článek neexistuje');
            $this->redirect('/admin/aktuality');
        }
        if (!$request->post('foto')) {
            if ($request->get('dir') === null) {
                if ($article['at_foto']) {
                    $this->redirect('/admin/aktuality/foto/' . $id . '?dir=' . $article['at_foto']);
                } else {
                    $request->get('dir', 0);
                }
            }

            if (!DBGalerie::getSingleDir($request->get('dir'))) {
                $this->redirect()->warning('Taková složka neexistuje');
                return $this->redirect('/admin/aktuality/foto/' . $id . '?dir=0');
            }

            $photos = array_map(fn($item) => [
                'id' => $item['gf_id'],
                'name' => $item['gf_name'],
                'src' => '/galerie/thumbnails/' . $item['gf_path']
            ], DBGalerie::getFotky($request->get('dir')));

            $dirs = DBGalerie::getDirs(true, true);
            $dirs_out = [];
            foreach ($dirs as $item) {
                $dirs_out[$item['gd_id']] = str_repeat("&nbsp;&nbsp;", $item['gd_level'] - 1) . $item['gd_name'];
            }

            return new \RenderHelper('files/View/Admin/Aktuality/FormFoto.inc', [
                'header' => 'Správa článků',
                'photos' => $photos,
                'dir' => $request->get('dir') ?: '',
                'dirs' => $dirs_out,
                'checked' => $article['at_foto_main']
            ]);
        }
        if ($request->get('dir') === null) {
            $request->get('dir', 0);
        }

        DBAktuality::editAktualita(
            $id,
            $article['at_kat'],
            $article['at_jmeno'],
            $article['at_text'],
            $article['at_preview'],
            $request->get('dir'),
            $request->post('foto'),
            $article['at_timestamp_add']
        );
        $this->redirect('/admin/aktuality');
    }
}
