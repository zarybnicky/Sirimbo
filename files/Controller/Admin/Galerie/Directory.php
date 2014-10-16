<?php
require_once 'files/Controller/Admin/Galerie.php';
class Controller_Admin_Galerie_Directory extends Controller_Admin_Galerie
{
    public function __construct()
    {
        Permissions::checkError('galerie', P_OWNED);
    }

    public function view($id = null)
    {
        if (!($data = DBGalerie::getSingleDir($id))) {
            $this->redirect('/admin/galerie', 'Složka s takovým ID neexistuje');
        }
        switch (post('action')) {
        	case 'file/edit':
        	    $galerie = post('galerie');
        	    if (isset($galerie[0])) {
        	        $this->redirect(
                        '/admin/galerie/file/' . post('action') . '/' . $galerie[0]
        	        );
        	    }
        	    break;
        	case 'file/remove':
                if (is_array(post('galerie'))) {
                    $this->redirect(
                        '/admin/galerie/file/remove?'
                            . http_build_query(array('u' => post('galerie')))
                    );
                }
        	    break;
        }
        $this->_displayOverview($id);
    }
    public function add($id = null)
    {
        if (empty($_POST) || is_object($form = $this->_checkData())) {
            if (!empty($_POST)) {
                $this->redirect()->setMessage($form->getMessages());
            }
            $this->_displayForm('add');
            return;
        }
        $parent = DBGalerie::getSingleDir(post('parent'));
        $dirPath = $parent['gd_path'] . DIRECTORY_SEPARATOR
            . $this->_sanitizePathname(post('name'));
        mkdir($dirPath, 0777, true);

        DBGalerie::addDir(
            post('name'), $parent['gd_id'], $parent['gd_level'] + 1,
            post('hidden') ? '1' : '0', $dirPath
        );
        $this->redirect('/admin/galerie', 'Složka přidána');
    }

    public function edit($id = null)
    {
        if ($id == 0) {
            $this->redirect('/admin/galerie', 'Není možné upravit hlavní složku');
        }
        if (!($data = DBGalerie::getSingleDir($id))) {
            $this->redirect('/admin/galerie', 'Taková složka neexistuje');
        }
        if (empty($_POST) || is_object($form = $this->_checkData())) {
            if (empty($_POST)) {
                post('name', $data['gd_name']);
                post('parent', $data['gd_id_rodic']);
                post('hidden', $data['gd_hidden'] ? '1' : '0');
            } else {
                $this->redirect()->setMessage($form->getMessages());
            }
            $this->_displayForm('edit');
            return;
        }
        $parent = DBGalerie::getSingleDir(post('parent'));
        $newPath = $this->_sanitizePathname(
            $this->_getCanonicalName(
                $parent['gd_path'] . DIRECTORY_SEPARATOR . post('name')
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
            $id, post('name'), $parent['gd_id'], $parent['gd_level'] + 1,
            post('hidden') ? '1' : '0', $data['gd_path']
        );
        $this->redirect('/admin/galerie', 'Složka byla úspěšně upravena.');
    }

    public function remove($id = null)
    {
        if (!is_array(post('data')) && !is_array(get('u'))) {
            $this->redirect('/admin/galerie');
        }
        if (!empty($_POST) && post('action') == 'confirm') {
            foreach (post('data') as $id) {
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
        $data = array();
        foreach (get('u') as $id) {
            $item = DBGalerie::getSingleDir($id);
            $data[] = array(
                'id' => $item['gd_id'],
                'text' => $item['gd_name']
            );
        }
        $this->render(
            'files/View/Admin/RemovePrompt.inc',
            array(
                'header' => 'Správa galerie',
                'prompt' => 'Opravdu chcete odstranit složky '
                    . 'se všemi podsložkami a fotkami:',
                'returnURI' => Request::getReferer(),
                'data' => $data
            )
        );
    }

    private function _displayOverview($id)
    {
        $data = DBGalerie::getFotky($id);
        foreach($data as &$item) {
            $newData = array(
                'id'           => $item['gf_id'],
        	    'checkBox'     => (string) $this->checkbox('galerie[]', $item['gf_id']),
                'name'         => $item['gf_name'],
                'thumbnailURI' => '/galerie/thumbnails/', $item['gf_path']
            );
            $item = $newData;
        }

        $this->render(
            'files/View/Admin/Galerie/DisplayDirectory.inc',
            array(
                'id' => $id,
                'files' => DBGalerie::getFotky($id)
            )
        );
    }

    private function _displayForm($action)
    {
        $dirs = DBGalerie::getDirs(true, true);
        foreach ($dirs as &$item) {
            $new_data = array(
                'id'    => $item['gd_id'],
                'text'  => str_repeat('&nbsp;&nbsp;', $item['gd_level'] - 1)
                . $item['gd_name']
            );
            $item = $new_data;
        }
        $this->render(
            'files/View/Admin/Galerie/FormDirectory.inc',
            array(
                'dirs' => $dirs,
                'action' => $action
            )
        );
        return;
    }

    private function _checkData()
    {
        $form = new Form();
        $form->checkNotEmpty(
            post('name'),
            'Název složky nesmí být prázdný', 'name'
        );
        $form->checkBool(
            post('parent') >= 0 && is_numeric(post('parent'))
            && DBGalerie::getSingleDir(post('parent')),
            'Zadaná nadsložka není platná', 'parent'
        );
        return $form->isValid() ? array() : $form;
    }
}
