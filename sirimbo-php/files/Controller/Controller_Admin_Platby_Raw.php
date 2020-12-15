<?php
class Controller_Admin_Platby_Raw extends Controller_Admin_Platby
{
    const TEMP_DIR = './upload/csv/';

    public function view()
    {
        \Permissions::checkError('platby', P_OWNED);
        if ($_POST && $_POST['action'] == 'upload') {
            static::processUpload();
        }
        $workDir = new \DirectoryIterator(self::TEMP_DIR);
        $workDir->rewind();
        foreach ($workDir as $fileInfo) {
            if (!$fileInfo->isFile()) {
                continue;
            }
            static::processCsv($fileInfo->getPathname());
            if ($path = $fileInfo->getRealPath()) {
                unlink($path);
            }
            new \MessageHelper('success', 'Soubor ' . $fileInfo->getFilename() . ' byl zpracován.');
        }

        new \RenderHelper('files/View/Admin/Platby/RawUpload.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Import plateb',
            'uri' => trim(explode('?', $_SERVER['REQUEST_URI'])[0], '/')
        ]);
    }

    public function select_columns()
    {
        \Permissions::checkError('platby', P_OWNED);
        $path = self::TEMP_DIR . str_replace('../', '', $_GET['path']);

        if ($_POST) {
            static::processCsv($path, [
                'specific' => $_POST['specific'],
                'variable' => $_POST['variable'],
                'date' => $_POST['date'],
                'amount' => $_POST['amount']
            ]);
            new \MessageHelper('success', 'Soubor ' . $_GET['path'] . ' byl zpracován.');
            new \RedirectHelper('/admin/platby/raw');
        }
        $parser = static::getParser($path);
        static::recognizeHeaders(array_flip($parser->headers()), $specific, $variable, $date, $amount);

        $data = [];
        foreach ($parser->headers() as $name) {
            $data[] = [
                'column' => $name,
                'specific' => new \RadioHelper('specific', $name, $name == $specific),
                'variable' => new \RadioHelper('variable', $name, $name == $variable),
                'date' => new \RadioHelper('date', $name, $name == $date),
                'amount' => new \RadioHelper('amount', $name, $name == $amount)
            ];
        }
        new \RenderHelper('files/View/Admin/Platby/RawColumnSelect.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Import plateb',
            'data' => $data,
            'uri' => trim(explode('?', $_SERVER['REQUEST_URI'])[0], '/')
        ]);
    }

    private static function getParser($path)
    {
        $fileinfo = new \SplFileInfo($path);
        if (!$fileinfo->isReadable()) {
            new \MessageHelper('danger', 'Soubor ' . str_replace(self::TEMP_DIR, '', $path) . ' není přístupný.');
            new \RedirectHelper('/admin/platby/raw');
        }
        $parser = new \CSVParser($fileinfo->openFile('r'));
        $parser->associative(true);
        return $parser;
    }

    private static function processCsv($path, $columns = null)
    {
        $parser = static::getParser($path);
        $headers = $parser->headers();
        if ($columns === null) {
            static::recognizeHeaders(array_flip($headers), $specific, $variable, $date, $amount);
        } else {
            $specific = $columns['specific'];
            $variable = $columns['variable'];
            $date = $columns['date'];
            $amount = $columns['amount'];
        }
        if (!static::checkHeaders($headers, $specific, $variable, $date, $amount)) {
            new \MessageHelper('info', 'Skript nemohl rozpoznat sloupce nutné pro zařazení plateb, je potřeba udělat to ručně. (soubor: ' . $path . ')');
            new \RedirectHelper('/admin/platby/raw/select_columns?path=' . str_replace(self::TEMP_DIR, '', $path));
        }
        $userLookup = static::getUserLookup(false);
        $categoryLookup = static::getCategoryLookup(true, true, false);

        foreach ($parser as $array) {
            if (!$array) {
                continue;
            }
            $serialized = serialize($array);
            $hash = md5($serialized);

            $item = new \PlatbyItem();
            $item->init($array[$specific], $array[$variable], $array[$date], $array[$amount]);
            $item->processWithSymbolLookup($userLookup, $categoryLookup);

            if (!$item->isValid) {
                \DBPlatbyRaw::insert($serialized, $hash, '0', '0', false);
                continue;
            } else {
                \DBPlatbyRaw::insert($serialized, $hash, '1', '0', true);
                \DBPlatbyItem::insert(
                    $item->variable,
                    $item->categoryId,
                    \DBPlatbyRaw::getInsertId(),
                    $item->amount,
                    $item->date,
                    $item->prefix
                );
            }
        }
    }

    private static function processUpload()
    {
        $upload = new \UploadHelper('in');
        $upload->loadFromPost();

        $validFiles = $upload->hasValidFiles();
        if ($upload->hasInvalidFiles()) {
            return new \MessageHelper('warning', $upload->getErrorMessages());
        } elseif ($upload->hasEmptyFiles() && empty($validFiles)) {
            return new \MessageHelper('info', 'Vyberte prosím nějaký soubor (prázdné soubory jsou automaticky odmítnuty).');
        }
        $uploader = $upload->getFilledUploader();
        $uploader->setOutputDir(self::TEMP_DIR);
        $uploader->addAllowedType('csv');
        $uploader->save();
        if ($uploader->hasRefusedFiles()) {
            new \MessageHelper('warning', 'Nahrávané soubory musí být typu CSV.');
        }
        foreach ($uploader->getSavedFiles() as $path) {
            static::processCsv($path);
            new \MessageHelper('success', 'Soubor ' . str_replace(self::TEMP_DIR, '', $path) . ' byl zpracován.');
        }
    }
}
