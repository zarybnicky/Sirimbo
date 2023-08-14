<?php
namespace Olymp\Controller\Admin;

class PlatbyRaw
{
    const TEMP_DIR = UPLOADS . '/csv/';

    public static function get()
    {
        if (!is_dir(self::TEMP_DIR)) {
            mkdir(self::TEMP_DIR, 0777, true);
        }
        \Permissions::checkError('platby', P_OWNED);
        $workDir = new \DirectoryIterator(self::TEMP_DIR);
        $workDir->rewind();
        foreach ($workDir as $fileInfo) {
            if (!$fileInfo->isFile()) {
                continue;
            }
            self::processCsv($fileInfo->getPathname());
            if ($path = $fileInfo->getRealPath()) {
                unlink($path);
            }
            \Message::success('Soubor ' . $fileInfo->getFilename() . ' byl zpracován.');
        }
        \Render::twig('Admin/PlatbyRawUpload.twig');
    }

    public static function post()
    {
        \Permissions::checkError('platby', P_OWNED);
        $uploader = new \Uploader($_FILES['in'], self::TEMP_DIR, ['csv']);
        if ($uploader->getUploadErrors()) {
            \Message::warning($uploader->getUploadErrors());
            return;
        }
        list($saved, $refused) = $uploader->save();
        if ($refused) {
            \Message::warning('Nahrávané soubory musí být typu CSV.');
        }
        foreach ($saved as $path) {
            self::processCsv($path);
            \Message::success('Soubor ' . str_replace(self::TEMP_DIR, '', $path) . ' byl zpracován.');
        }
        \Redirect::to('/platby/raw');
    }

    public static function selectColumns()
    {
        \Permissions::checkError('platby', P_OWNED);
        $headers = self::getParser(self::TEMP_DIR . str_replace('../', '', $_GET['path']))[0];
        Platby::recognizeHeaders(array_flip($headers), $specific, $variable, $date, $amount);
        \Render::twig('Admin/PlatbyRawColumnSelect.twig', [
            'data' => $headers,
            'recognized' => [
                'specific' => $specific,
                'variable' => $variable,
                'date' => $date,
                'amount' => $amount,
            ],
        ]);
    }

    public static function selectColumnsPost()
    {
        \Permissions::checkError('platby', P_OWNED);
        self::processCsv(self::TEMP_DIR . str_replace('../', '', $_GET['path']), [
            'specific' => $_POST['specific'],
            'variable' => $_POST['variable'],
            'date' => $_POST['date'],
            'amount' => $_POST['amount'],
        ]);
        \Message::success('Soubor ' . $_GET['path'] . ' byl zpracován.');
        \Redirect::to('/platby/raw');
    }

    private static function getParser($path)
    {
        $fileinfo = new \SplFileInfo($path);
        if (!$fileinfo->isReadable()) {
            \Message::danger('Soubor ' . str_replace(self::TEMP_DIR, '', $path) . ' není přístupný.');
            \Redirect::to('/platby/raw');
        }
        $text = file_get_contents($path);
        $text = mb_convert_encoding($text, 'UTF8', 'UTF-16LE');
        $lines = preg_split("/\R/", $text);
        $lines = array_map(fn($x) => str_getcsv($x, ';'), $lines);
        $headers = array_shift($lines);
        if ($lines[count($lines) - 1][0] == null) {
            array_pop($lines);
        }
        return [$headers, array_map(fn($x) => array_combine($headers, $x), $lines)];
    }

    private static function processCsv($path, $columns = null)
    {
        [$headers, $lines] = self::getParser($path);
        if ($columns === null) {
            Platby::recognizeHeaders(array_flip($headers), $specific, $variable, $date, $amount);
        } else {
            $specific = $columns['specific'];
            $variable = $columns['variable'];
            $date = $columns['date'];
            $amount = $columns['amount'];
        }
        if (!Platby::checkHeaders($headers, $specific, $variable, $date, $amount)) {
            \Message::info("Skript nemohl rozpoznat sloupce nutné pro zařazení plateb, je potřeba udělat to ručně. (soubor: $path)");
            \Redirect::to('/platby/raw/select_columns?path=' . str_replace(self::TEMP_DIR, '', $path));
        }
        $userLookup = Platby::getUserLookup(false);
        $categoryLookup = Platby::getCategoryLookup(true, true, false);

        foreach ($lines as $array) {
            $serialized = serialize($array);
            $hash = md5($serialized);
            $row = \Database::query(
                "INSERT INTO platby_raw (pr_raw,pr_hash,pr_sorted,pr_discarded) VALUES ('?','?','?','?')
                ON CONFLICT (pr_hash) DO UPDATE SET pr_discarded=false, pr_sorted=EXCLUDED.pr_sorted
                RETURNING pr_id",
                $serialized,
                $hash,
                '0',
                '0',
            );
            $rawId = $row['pr_id'];

            $item = new \PlatbyItem($array[$specific], $array[$variable], $array[$date], $array[$amount]);
            $item->processWithSymbolLookup($userLookup, $categoryLookup);
            if ($item->isValid) {
                \Database::query("UPDATE platby_raw SET pr_sorted='1' where pr_id='?'", $rawId);
                \DBPlatbyItem::insert(
                    $item->variable,
                    $item->categoryId,
                    $rawId,
                    $item->amount,
                    $item->date,
                    $item->prefix
                );
            }
        }
    }
}
