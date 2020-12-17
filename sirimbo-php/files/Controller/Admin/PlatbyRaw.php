<?php
namespace Olymp\Controller\Admin;

class PlatbyRaw
{
    const TEMP_DIR = './upload/csv/';

    public static function get()
    {
        \Permissions::checkError('platby', P_OWNED);
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

    public static function post()
    {
        \Permissions::checkError('platby', P_OWNED);
        static::processUpload();
        new \RedirectHelper('/admin/platby/raw');
    }

    public static function selectColumns()
    {
        \Permissions::checkError('platby', P_OWNED);
        $path = self::TEMP_DIR . str_replace('../', '', $_GET['path']);
        $parser = static::getParser($path);
        Platby::recognizeHeaders(array_flip($parser->headers()), $specific, $variable, $date, $amount);

        $data = array_map(
            fn($name) => [
                'column' => $name,
                'specific' => new \RadioHelper('specific', $name, $name == $specific),
                'variable' => new \RadioHelper('variable', $name, $name == $variable),
                'date' => new \RadioHelper('date', $name, $name == $date),
                'amount' => new \RadioHelper('amount', $name, $name == $amount)
            ],
            $parser->headers()
        );
        new \RenderHelper('files/View/Admin/Platby/RawColumnSelect.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Import plateb',
            'data' => $data,
            'uri' => trim(explode('?', $_SERVER['REQUEST_URI'])[0], '/')
        ]);
    }

    public static function selectColumnsPost()
    {
        \Permissions::checkError('platby', P_OWNED);
        $path = self::TEMP_DIR . str_replace('../', '', $_GET['path']);
        static::processCsv($path, [
            'specific' => $_POST['specific'],
            'variable' => $_POST['variable'],
            'date' => $_POST['date'],
            'amount' => $_POST['amount']
        ]);
        new \MessageHelper('success', 'Soubor ' . $_GET['path'] . ' byl zpracován.');
        new \RedirectHelper('/admin/platby/raw');
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
            Platby::recognizeHeaders(array_flip($headers), $specific, $variable, $date, $amount);
        } else {
            $specific = $columns['specific'];
            $variable = $columns['variable'];
            $date = $columns['date'];
            $amount = $columns['amount'];
        }
        if (!Platby::checkHeaders($headers, $specific, $variable, $date, $amount)) {
            new \MessageHelper('info', "Skript nemohl rozpoznat sloupce nutné pro zařazení plateb, je potřeba udělat to ručně. (soubor: $path)");
            new \RedirectHelper('/admin/platby/raw/select_columns?path=' . str_replace(self::TEMP_DIR, '', $path));
        }
        $userLookup = Platby::getUserLookup(false);
        $categoryLookup = Platby::getCategoryLookup(true, true, false);

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
            }
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
