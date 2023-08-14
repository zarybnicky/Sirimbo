<?php
namespace Olymp\Controller\Admin;

class GalerieFile
{
    public static function remove($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        $item = \Database::querySingle("SELECT * FROM galerie_foto WHERE gf_id='?'", $id);
        \Render::twig('RemovePrompt.twig', [
            'header' => 'Správa galerie',
            'prompt' => 'Opravdu chcete odstranit fotografie:',
            'returnURI' => $_SERVER['HTTP_REFERER'] ?? '/galerie',
            'data' => [['id' => $id, 'text' => $item['gf_name']]]
        ]);
    }

    public static function removePost($id)
    {
        \Permissions::checkError('galerie', P_OWNED);
        $item = \Database::querySingle("SELECT * FROM galerie_foto WHERE gf_id='?'", $id);
        \Database::query("DELETE FROM galerie_foto WHERE gf_id='?'", $id);
        unlink(GALERIE . DIRECTORY_SEPARATOR . $item['gf_path']);
        unlink(GALERIE_THUMBS . DIRECTORY_SEPARATOR . $item['gf_path']);
        \Redirect::to('/galerie');
    }

    public static function upload()
    {
        \Permissions::checkError('galerie', P_OWNED);
        \Render::twig('Admin/GalerieUpload.twig', [
            'dir' => $_GET['dir'] ?? '1',
            'dirs' => array_map(
                fn($item) => [
                    'id' => $item['gd_id'],
                    'text' => str_repeat('&nbsp;&nbsp;', $item['gd_level'] - 1) . $item['gd_name']
                ],
                \DBGalerie::getDirs(),
            ),
        ]);
    }

    public static function uploadPost()
    {
        \Permissions::checkError('galerie', P_OWNED);
        $parentId = $_POST['dir'];
        if (!is_numeric($parentId) || $parentId < 1) {
            $parentId = 1;
        }
        $parent = \Database::querySingle("SELECT * FROM galerie_dir WHERE gd_id='?'", $parentId);
        if (!$parent) {
            \Message::warning('Taková složka neexistuje');
            \Redirect::to('/galerie/file/upload');
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
            \Redirect::to('/galerie/file/upload');
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

            \Database::query(
                "INSERT INTO galerie_foto (gf_id_rodic,gf_path,gf_name,gf_kdo) VALUES ('?','?','?','?')",
                $parent['gd_id'],
                $path,
                $name,
                \Session::getUser()->getId(),
            );
        }
        if ($failCount > 0) {
            \Message::warning("Počet neúspěšně zpracovaných souborů: $failCount");
        }
        if (count($uploader->getSavedFiles()) > $failCount) {
            \Message::info('Fotky přidány');
            \Redirect::to('/galerie');
        }
        \Message::info('Počet nahraných souborů: ' . count($uploader->getSavedFiles()));
        \Redirect::to('/galerie');
    }
}
