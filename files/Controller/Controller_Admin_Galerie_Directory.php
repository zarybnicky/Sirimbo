<?php
class Controller_Admin_Galerie_Directory extends Controller_Abstract
{
    public function __construct()
    {
        Permissions::checkError('galerie', P_OWNED);
    }

    public function view($request)
    {
        $id = $request->getId();
        if (!DBGalerie::getSingleDir($id)) {
            $this->redirect()->warning('Složka s takovým ID neexistuje');
            $this->redirect('/admin/galerie');
        }

        $this->render('files/View/Admin/Galerie/DisplayDirectory.inc', [
            'header' => 'Správa fotogalerie',
            'id' => $id,
            'files' => array_map(
                function ($item) {
                    return [
                        'id' => $item['gf_id'],
                        'name' => $item['gf_name'],
                        'thumbnailURI' => '/galerie/thumbnails/' . $item['gf_path']
                    ];
                },
                DBGalerie::getFotky($id)
            )
        ]);
    }

    public function add($request)
    {
        if (!$request->post()) {
            $this->displayForm($request, 'add');
            return;
        }
        if (is_object($form = $this->checkData($request))) {
            $this->redirect()->warning($form->getMessages());
            $this->displayForm($request, 'add');
            return;
        }
        $parent = DBGalerie::getSingleDir($request->post('parent'));
        $dirPath = $parent['gd_path'] . DIRECTORY_SEPARATOR
                 . sanitizePathname($request->post('name'));
        mkdir($dirPath, 0777, true);

        DBGalerie::addDir(
            $request->post('name'),
            $parent['gd_id'],
            $parent['gd_level'] + 1,
            $request->post('hidden') ? '1' : '0',
            $dirPath
        );
        $this->redirect('/admin/galerie');
    }

    public function edit($request)
    {
        $id = $request->getId();
        if (!$id) {
            $this->redirect()->warning('Není možné upravit hlavní složku');
            $this->redirect('/admin/galerie');
        }
        if (!($data = DBGalerie::getSingleDir($id))) {
            $this->redirect()->warning('Taková složka neexistuje');
            $this->redirect('/admin/galerie');
        }
        if (!$request->post()) {
            $request->post('name', $data['gd_name']);
            $request->post('parent', $data['gd_id_rodic']);
            $request->post('hidden', $data['gd_hidden'] ? '1' : '0');
            $this->displayForm($request, 'edit');
            return;
        }
        if (is_object($form = $this->checkData($request))) {
            $this->redirect()->warning($form->getMessages());
            $this->displayForm($request, 'edit');
            return;
        }
        $parent = DBGalerie::getSingleDir($request->post('parent'));
        $newPath = $parent['gd_path'] . DIRECTORY_SEPARATOR
                 . sanitizePathname(
                     getCanonicalName($request->post('name'))
                 );

        if ($data['gd_path'] != $newPath) {
            if (file_exists(GALERIE . DIRECTORY_SEPARATOR . $newPath)) {
                $this->redirect()->danger('V dané nadsložce už existuje složka se stejným názvem.');
                $this->redirect('/admin/galerie/directory/edit/' . $id);
            }
            rename(
                GALERIE . DIRECTORY_SEPARATOR . $data['gd_path'],
                GALERIE . DIRECTORY_SEPARATOR . $newPath
            );
            rename(
                GALERIE_THUMBS . DIRECTORY_SEPARATOR . $data['gd_path'],
                GALERIE_THUMBS . DIRECTORY_SEPARATOR . $newPath
            );
            DBGalerie::editFotoReplacePath($id, $data['gd_path'], $newPath);
            $data['gd_path'] = $newPath;
        }

        DBGalerie::editDir(
            $id,
            $request->post('name'),
            $parent['gd_id'],
            $parent['gd_level'] + 1,
            $request->post('hidden') ? '1' : '0',
            $data['gd_path']
        );
        $this->redirect('/admin/galerie');
    }

    public function remove($request)
    {
        if (!$request->getId()) {
            $this->redirect('/admin/galerie');
        }
        $id = $request->getId();

        if ($request->post('action') == 'confirm') {
            $data = DBGalerie::getSingleDir($id);
            DBGalerie::removeDir($id);
            if ($data['gd_path']) {
                rrmdir(GALERIE . DIRECTORY_SEPARATOR . $data['gd_path']);
                rrmdir(GALERIE_THUMBS . DIRECTORY_SEPARATOR . $data['gd_path']);
            }
            $this->redirect('/admin/galerie');
        }

        $item = DBGalerie::getSingleDir($id);
        $this->render('files/View/Admin/RemovePrompt.inc', [
            'header' => 'Správa galerie',
            'prompt' => 'Opravdu chcete odstranit složky se všemi podsložkami a fotkami:',
            'returnURI' => $request->getReferer() ?: '/admin/galerie',
            'data' => [['id' => $item['gd_id'], 'text' => $item['gd_name']]]
        ]);
    }

    private function displayForm($request, $action)
    {
        $dirs = array_map(
            function ($item) {
                return [
                    'id'    => $item['gd_id'],
                    'text'  => str_repeat('&nbsp;&nbsp;', $item['gd_level'] - 1)
                    . $item['gd_name']
                ];
            },
            DBGalerie::getDirs(true, true)
        );

        $this->render('files/View/Admin/Galerie/FormDirectory.inc', [
            'header' => 'Správa fotogalerie',
            'subheader' => ($action == 'add' ? 'Přidat' : 'Upravit') . ' složku',
            'dirs' => $dirs,
            'action' => $action,
            'name' => $request->post('name') ?: '',
            'parent' => $request->post('parent') ?: '',
            'hidden' => $request->post('hidden') ?: ''
        ]);
        return;
    }

    protected function checkData($request)
    {
        $form = new Form();
        $form->checkNotEmpty(
            $request->post('name'),
            'Název složky nesmí být prázdný',
            'name'
        );
        $form->checkBool(
            $request->post('parent') >= 0
            && is_numeric($request->post('parent'))
            && DBGalerie::getSingleDir($request->post('parent')),
            'Zadaná nadsložka není platná',
            'parent'
        );
        return $form->isValid() ? [] : $form;
    }
}
