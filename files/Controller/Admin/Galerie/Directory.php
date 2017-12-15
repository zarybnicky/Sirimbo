<?php
require_once 'files/Controller/Admin/Galerie.php';
class Controller_Admin_Galerie_Directory extends Controller_Admin_Galerie
{
    public function __construct()
    {
        Permissions::checkError('galerie', P_OWNED);
    }

    public function view($request)
    {
        $id = $request->getId();
        if (!($data = DBGalerie::getSingleDir($id))) {
            $this->redirect('/admin/galerie', 'Složka s takovým ID neexistuje');
        }
        switch ($request->post('action')) {
        case 'file/edit':
            $galerie = $request->post('galerie');
            if (isset($galerie[0])) {
                $this->redirect(
                    '/admin/galerie/file/edit/' . $galerie[0]
                );
            }
            break;
        case 'file/remove':
            if (is_array($request->post('galerie'))) {
                $this->redirect(
                    '/admin/galerie/file/remove?'
                    . http_build_query(['u' => $request->post('galerie')])
                );
            }
            break;
        }
        $this->displayOverview($id);
    }

    public function add($request)
    {
        if (!$request->post()) {
            $this->displayForm($request, 'add');
            return;
        }
        if (is_object($form = $this->checkData($request))) {
            $this->redirect()->setMessage($form->getMessages());
            $this->displayForm($request, 'add');
            return;
        }
        $parent = DBGalerie::getSingleDir($request->post('parent'));
        $dirPath = $parent['gd_path'] . DIRECTORY_SEPARATOR
                 . $this->_sanitizePathname($request->post('name'));
        mkdir($dirPath, 0777, true);

        DBGalerie::addDir(
            $request->post('name'),
            $parent['gd_id'],
            $parent['gd_level'] + 1,
            $request->post('hidden') ? '1' : '0',
            $dirPath
        );
        $this->redirect('/admin/galerie', 'Složka přidána');
    }

    public function edit($request)
    {
        $id = $request->getId();
        if (!$id) {
            $this->redirect('/admin/galerie', 'Není možné upravit hlavní složku');
        }
        if (!($data = DBGalerie::getSingleDir($id))) {
            $this->redirect('/admin/galerie', 'Taková složka neexistuje');
        }
        if (!$request->post()) {
            $request->post('name', $data['gd_name']);
            $request->post('parent', $data['gd_id_rodic']);
            $request->post('hidden', $data['gd_hidden'] ? '1' : '0');
            $this->displayForm($request, 'edit');
            return;
        }
        if (is_object($form = $this->checkData($request))) {
            $this->redirect()->setMessage($form->getMessages());
            $this->displayForm($request, 'edit');
            return;
        }
        $parent = DBGalerie::getSingleDir($request->post('parent'));
        $newPath = $parent['gd_path'] . DIRECTORY_SEPARATOR
                 . $this->_sanitizePathname(
                     $this->_getCanonicalName(
                         $request->post('name')
                     )
                 );

        if ($data['gd_path'] != $newPath) {
            if (file_exists(GALERIE . DIRECTORY_SEPARATOR . $newPath)) {
                $this->redirect(
                    '/admin/galerie/directory/edit/' . $id,
                    'V dané nadsložce už existuje složka se stejným názvem.'
                );
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
        $this->redirect('/admin/galerie', 'Složka byla úspěšně upravena.');
    }

    public function remove($request)
    {
        if (!is_array($request->post('data')) && !is_array($request->get('u'))) {
            $this->redirect('/admin/galerie');
        }
        if ($request->post('action') == 'confirm') {
            foreach ($request->post('data') as $id) {
                if (!($data = DBGalerie::getSingleDir($id))) {
                    continue;
                }
                DBGalerie::removeDir($id);
                if(!$data['gd_path']) {
                    continue;
                }
                $this->_rrmdir(GALERIE . DIRECTORY_SEPARATOR . $data['gd_path']);
                $this->_rrmdir(GALERIE_THUMBS . DIRECTORY_SEPARATOR . $data['gd_path']);
            }
            $this->redirect('/admin/galerie', 'Složky odebrány');
        }
        $data = array_map(
            function ($id) {
                $item = DBGalerie::getSingleDir($id);
                return ['id' => $item['gd_id'], 'text' => $item['gd_name']];
            },
            $request->get('u')
        );
        $this->render(
            'files/View/Admin/RemovePrompt.inc',
            [
                'header' => 'Správa galerie',
                'prompt' => 'Opravdu chcete odstranit složky '
                    . 'se všemi podsložkami a fotkami:',
                'returnURI' => $request->getReferer(),
                'data' => $data
            ]
        );
    }

    private function displayOverview($id)
    {
        $this->render(
            'files/View/Admin/Galerie/DisplayDirectory.inc',
            [
                'id' => $id,
                'files' => array_map(
                    function ($item) {
                        return [
                            'id' => $item['gf_id'],
                            'name' => $this->checkbox('galerie[]', $item['gf_id'])
                                           ->label($item['gf_name'])
                                           ->render(),
                            'thumbnailURI' => '/galerie/thumbnails/' . $item['gf_path']
                        ];
                    },
                    DBGalerie::getFotky($id)
                )
            ]
        );
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

        $this->render(
            'files/View/Admin/Galerie/FormDirectory.inc',
            [
                'dirs' => $dirs,
                'action' => $action,
                'name' => $request->post('name') ?: '',
                'parent' => $request->post('parent') ?: '',
                'hidden' => $request->post('hidden') ?: ''
            ]
        );
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
