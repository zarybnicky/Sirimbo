<?php
class Controller_Admin_Platby_Raw extends Controller_Admin_Platby
{
    const TEMP_DIR = './upload/csv/';

    public function __construct($request)
    {
        parent::__construct($request);
        Permissions::checkError('platby', P_OWNED);
    }

    public function view($request)
    {
        if ($request->post() && $request->post('action') == 'upload') {
            $this->processUpload($request);
        }
        $workDir = new DirectoryIterator(self::TEMP_DIR);
        $workDir->rewind();
        foreach ($workDir as $fileInfo) {
            if (!$fileInfo->isFile()) {
                continue;
            }
            $this->processCsv($fileInfo->getPathname());
            if ($path = $fileInfo->getRealPath()) {
                unlink($path);
            }
            $this->redirect()->success('Soubor ' . $fileInfo->getFilename() . ' byl zpracován.');
        }

        $this->render('files/View/Admin/Platby/RawUpload.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Import plateb',
            'uri' => $request->getLiteralURI()
        ]);
    }
    public function select_columns($request)
    {
        $path = self::TEMP_DIR . str_replace('../', '', $request->get('path'));

        if ($request->post()) {
            $this->processCsv(
                $path,
                [
                    'specific' => $request->post('specific'),
                    'variable' => $request->post('variable'),
                    'date' => $request->post('date'),
                    'amount' => $request->post('amount')
                ]
            );
            $this->redirect()->success('Soubor ' . $request->get('path') . ' byl zpracován.');
            $this->redirect('/admin/platby/raw');
        }
        $parser = $this->getParser($path);
        $this->recognizeHeaders(array_flip($parser->headers()), $specific, $variable, $date, $amount);

        $data = [];
        foreach ($parser->headers() as $name) {
            $data[] = [
                'column' => $name,
                'specific' => $this->radio('specific', $name)->set($name == $specific),
                'variable' => $this->radio('variable', $name)->set($name == $variable),
                'date' => $this->radio('date', $name)->set($name == $date),
                'amount' => $this->radio('amount', $name)->set($name == $amount)
            ];
        }
        $this->render('files/View/Admin/Platby/RawColumnSelect.inc', [
            'header' => 'Správa plateb',
            'subheader' => 'Import plateb',
            'data' => $data,
            'uri' => $request->getLiteralURI()
        ]);
    }
    private function getParser($path)
    {
        $fileinfo = new SplFileInfo($path);
        if (!$fileinfo->isReadable()) {
            $this->redirect()->danger('Soubor ' . str_replace(self::TEMP_DIR, '', $path) . ' není přístupný.');
            $this->redirect('/admin/platby/raw');
        }
        $parser = new CSVParser($fileinfo->openFile('r'));
        $parser->associative(true);
        return $parser;
    }

    private function processCsv($path, $columns = null)
    {
        $parser = $this->getParser($path);
        $headers = $parser->headers();
        if ($columns === null) {
            $this->recognizeHeaders(array_flip($headers), $specific, $variable, $date, $amount);
        } else {
            $specific = $columns['specific'];
            $variable = $columns['variable'];
            $date = $columns['date'];
            $amount = $columns['amount'];
        }
        if (!$this->checkHeaders($headers, $specific, $variable, $date, $amount)) {
            $this->redirect()->info('Skript nemohl rozpoznat sloupce nutné pro zařazení plateb, je potřeba udělat to ručně. (soubor: ' . $path . ')');
            $this->redirect('/admin/platby/raw/select_columns?path=' . str_replace(self::TEMP_DIR, '', $path));
        }
        $userLookup = $this->getUserLookup(false);
        $categoryLookup = $this->getCategoryLookup(true, true, false);

        foreach ($parser as $array) {
            if (!$array) {
                continue;
            }
            $serialized = serialize($array);
            $hash = md5($serialized);

            $item = new PlatbyItem();
            $item->init($array[$specific], $array[$variable], $array[$date], $array[$amount]);
            $item->processWithSymbolLookup($userLookup, $categoryLookup);

            if (!$item->isValid) {
                DBPlatbyRaw::insert($serialized, $hash, '0', '0', false);
                continue;
            } else {
                DBPlatbyRaw::insert($serialized, $hash, '1', '0', true);
                DBPlatbyItem::insert(
                    $item->variable,
                    $item->categoryId,
                    DBPlatbyRaw::getInsertId(),
                    $item->amount,
                    $item->date,
                    $item->prefix
                );
            }
        }
    }

    private function processUpload($request)
    {
        $upload = new UploadHelper();
        $upload->upload('in')->loadFromPost($request);

        $validFiles = $upload->hasValidFiles();
        if ($upload->hasInvalidFiles()) {
            $this->redirect()->warning($upload->getErrorMessages());
            return;
        } elseif ($upload->hasEmptyFiles() && empty($validFiles)) {
            $this->redirect()->info('Vyberte prosím nějaký soubor (prázdné soubory jsou automaticky odmítnuty).');
            return;
        }
        $uploader = $upload->getFilledUploader();
        $uploader->setOutputDir(self::TEMP_DIR);
        $uploader->addAllowedType('csv');
        $uploader->save();
        if ($uploader->hasRefusedFiles()) {
            $this->redirect()->warning('Nahrávané soubory musí být typu CSV.');
        }
        foreach ($uploader->getSavedFiles() as $path) {
            $this->processCsv($path);
            $this->redirect()->success('Soubor ' . str_replace(self::TEMP_DIR, '', $path) . ' byl zpracován.');
        }
    }
}
