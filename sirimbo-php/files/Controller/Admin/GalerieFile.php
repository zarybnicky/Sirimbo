<?php
namespace Olymp\Controller\Admin;

class GalerieFile
{
    public static function edit($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        if (!$data = \DBGalerie::getSingleFoto($id)) {
            \Message::warning('Takový soubor neexistuje!');
            \Redirect::to($_SERVER['HTTP_REFERER']);
        }
        $_POST['name'] = $data['gf_name'];
        $_POST['parent'] = $data['gf_id_rodic'];
        return static::displayForm($id);
    }

    public static function editPost($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        if (!$data = \DBGalerie::getSingleFoto($id)) {
            \Message::warning('Takový soubor neexistuje!');
            \Redirect::to($_SERVER['HTTP_REFERER']);
        }

        $form = new \Form();
        $form->checkNotEmpty($_POST['name'], 'Zadejte prosím nějaký popis');
        $form->checkBool(
            $_POST['parent'] > 0 && is_numeric($_POST['parent'])
            && \DBGalerie::getSingleDir($_POST['parent']),
            'Zadaná nadsložka není platná',
        );
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            return static::displayForm($id);
        }

        $parent = \DBGalerie::getSingleDir($_POST['parent']);
        $newPath = Galerie::sanitizePathname(
            Galerie::getCanonicalName(
                $parent['gd_path'] . DIRECTORY_SEPARATOR . $_POST['name']
            )
        );
        if ($data['gf_path'] != $newPath) {
            if (file_exists(GALERIE . DIRECTORY_SEPARATOR . $newPath)) {
                \Message::danger('V dané složce už existuje soubor se stejným názvem.');
                \Redirect::to('/admin/galerie/file/edit/' . $id);
            }
            rename(
                GALERIE . DIRECTORY_SEPARATOR . $data['gd_path'],
                GALERIE . DIRECTORY_SEPARATOR . $newPath
            );
            rename(
                GALERIE_THUMBS . DIRECTORY_SEPARATOR . $data['gd_path'],
                GALERIE_THUMBS . DIRECTORY_SEPARATOR . $newPath
            );
            $data['gf_path'] = $newPath;
        }
        \DBGalerie::editFoto($id, $data['gf_path'], $_POST['parent'], $_POST['name']);
        \Redirect::to('/admin/galerie/directory/' . $_POST['parent']);
    }

    public static function remove($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        $item = \DBGalerie::getSingleFoto($id);
        \Render::twig('RemovePrompt.twig', [
            'header' => 'Správa galerie',
            'prompt' => 'Opravdu chcete odstranit fotografie:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/admin/galerie',
            'data' => [['id' => $id, 'text' => $item['gf_name']]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        $item = \DBGalerie::getSingleFoto($id);
        \DBGalerie::removeFoto($id);
        unlink(GALERIE . DIRECTORY_SEPARATOR . $item['gf_path']);
        unlink(GALERIE_THUMBS . DIRECTORY_SEPARATOR . $item['gf_path']);
        \Redirect::to('/admin/galerie');
    }

    public static function upload()
    {
        \Permissions::checkError('galerie', P_OWNED);
        \Render::twig('Admin/GalerieUpload.twig', [
            'dir' => $_GET['dir'] ?? '1',
            'dirs' => array_for(\DBGalerie::getDirs(), fn($item) => [
                'id' => $item['gd_id'],
                'text' => str_repeat('&nbsp;&nbsp;', $item['gd_level'] - 1) . $item['gd_name']
            ]),
        ]);
    }

    public static function uploadPost()
    {
        \Permissions::checkError('galerie', P_OWNED);
        $parentId = $_POST['dir'];
        if (!is_numeric($parentId) || $parentId < 1) {
            $parentId = 1;
        }
        if (!($parent = \DBGalerie::getSingleDir($parentId))) {
            \Message::warning('Taková složka neexistuje');
            \Redirect::to('/admin/galerie/file/upload');
        }

        $outputDir = GALERIE . DIRECTORY_SEPARATOR . $parent['gd_path'];
        $uploader = new \Uploader($_FILES['files'], $outputDir, \Settings::$imageType);
        if ($uploader->getUploadErrors()) {
            \Message::warning($uploader->getUploadErrors());
        }
        list($saved, $refused) = $uploader->save();

        if ($refused) {
            \Message::warning('Počet zamítnutých souborů: ' . count($refused));
        }
        if (!$saved) {
            \Message::info('Žádné soubory nebyly nahrány!');
            \Redirect::to('/admin/galerie/file/upload');
        }
        $failCount = 0;
        foreach ($saved as $path) {
            if (!Galerie::checkGetThumbnail($path)) {
                if (is_file($path)) {
                    unlink($path);
                }
                $failCount++;
                continue;
            }
            $path = str_replace(GALERIE . DIRECTORY_SEPARATOR, '', $path);
            $parts = explode(DIRECTORY_SEPARATOR, $path);
            $name = array_pop($parts);

            \DBGalerie::addFoto($parent['gd_id'], $path, $name, \Session::getUser()->getId());
        }
        if ($failCount > 0) {
            \Message::warning("Počet neúspěšně zpracovaných souborů: $failCount");
        }
        if (count($uploader->getSavedFiles()) > $failCount) {
            \Message::info('Fotky přidány');
            \Redirect::to('/admin/galerie');
        }
        \Message::info('Počet nahraných souborů: ' . count($uploader->getSavedFiles()));
        \Redirect::to('/admin/galerie');
    }

    private static function displayForm($id)
    {
        \Render::twig('Admin/GalerieFormFile.twig', [
            'id' => $id,
            'returnURI' => $_SERVER['HTTP_REFERER'],
            'parent' => $_POST['parent'],
            'name' => $_POST['name'],
            'dirs' => [['id' => '1', 'text' => '---']] + array_map(
                fn($item) => [
                    'id' => $item['gd_id'],
                    'text' => str_repeat('&nbsp;&nbsp;', $item['gd_level'] - 1) . $item['gd_name']
                ],
                \DBGalerie::getDirs()
            ),
        ]);
    }
}
