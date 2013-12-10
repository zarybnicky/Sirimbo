<?php
namespace TKOlomouc\Controller\Admin\Platby;

use TKOlomouc\Controller\Admin\Platby;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Utility\CSVParser;
use TKOlomouc\View\Exception\ViewException;
use TKOlomouc\View\Helper\Upload;
use TKOlomouc\Type\PlatbyItem;
use TKOlomouc\Model\DBPlatbyRaw;
use TKOlomouc\Model\DBPlatbyItem;

class Raw extends Platby
{
    const TEMP_DIR = './upload/csv/';

    public function __construct()
    {
        Permissions::checkError('platby', P_OWNED);
    }

    public function view($id = null)
    {
        if (!empty($_POST) && post('action') == 'upload') {
            $this->processUpload();
        }
        $workDir = new \DirectoryIterator(self::TEMP_DIR);
        $workDir->rewind();
        foreach ($workDir as $fileInfo) {
            if (!$fileInfo->isFile()) {
                continue;
            }
            $this->processCsv($fileInfo->getPathname());
            $this->redirect()->setMessage('Soubor ' . $fileInfo->getFilename() . ' byl zpracován.');
        }

        $this->render('src/application/View/Admin/Platby/RawUpload.inc');
    }

    public function select_columns($id = null)
    {
        $path = self::TEMP_DIR . str_replace('../', '', get('path'));

        if (!empty($_POST)) {
            $this->processCsv($path, array(
                'specific' => post('specific'),
                'variable' => post('variable'),
                'date' => post('date'),
                'amount' => post('amount')
            ));
            $this->redirect('/admin/platby/raw', 'Soubor ' . get('path') . ' byl zpracován.');
        }
        $parser = $this->getParser($path);
        $this->recognizeHeaders(array_flip($parser->getHeaders()), $specific, $variable, $date, $amount);

        $data = array();
        foreach ($parser->getHeaders() as $key => $name) {
            $data[] = array(
                    'column' => $name,
                    'specific' => getRadio('specific', $name, $name == $specific),
                    'variable' => getRadio('variable', $name, $name == $variable),
                    'date' => getRadio('date', $name, $name == $date),
                    'amount' => getRadio('amount', $name, $name == $amount)
            );
        }
        $this->render('src/application/View/Admin/Platby/RawColumnSelect.inc', array(
                'data' => $data
        ));
    }

    private function getParser($path)
    {
        $fileinfo = new \SplFileInfo($path);

        if (!$fileinfo->isReadable()) {
            $this->redirect(
                '/admin/platby/raw',
                'Soubor ' . str_replace(self::TEMP_DIR, '', $path) . ' není přístupný.'
            );
        }
        $parser = new CSVParser($fileinfo->openFile('r'));
        $parser->setAssociative(true);
        return $parser;
    }

    private function processCsv($parser, $columns = null)
    {
        if (!is_a($parser, 'CSVParser')) {
            if (!is_string((string) $parser)) {
                throw new ViewException('$parser (' . gettype($parser) . ') is not a CSVParser or a string');
            }
            $parser = $this->getParser($parser);
        }
        $headers = $parser->getHeaders();
        if ($columns === null) {
            $this->recognizeHeaders(array_flip($headers), $specific, $variable, $date, $amount);
        } else {
            $specific = $columns['specific'];
            $variable = $columns['variable'];
            $date = $columns['date'];
            $amount = $columns['amount'];
        }
        if (!$this->checkHeaders($headers, $specific, $variable, $date, $amount)) {
            $this->redirect(
                '/admin/platby/raw/select_columns?path=' . str_replace(self::TEMP_DIR, '', $path),
                'Skript nemohl rozpoznat sloupce nutné pro zařazení plateb,'
                . ' je potřeba udělat to ručně. (soubor: ' . $path . ')'
            );
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
                    $item->category_id,
                    DBPlatbyRaw::getInsertId(),
                    $item->amount,
                    $item->date,
                    $item->prefix
                );
            }
        }
        unlink($parser->getFileObject()->getRealPath());
    }

    private function processUpload()
    {
        $upload = new Upload('in');
        $upload->loadFromPost();

        $validFiles = $upload->hasValidFiles();
        if ($upload->hasInvalidFiles()) {
            $this->redirect()->setMessage($upload->getErrorMessages());
            return;
        } elseif ($upload->hasEmptyFiles() && empty($validFiles)) {
            $this->redirect()->setMessage('Vyberte prosím nějaký soubor (prázdné soubory jsou automaticky odmítnuty).');
            return;
        }
        $uploader = $upload->getFilledUploadHandler();
        $uploader->setOutputDir(self::TEMP_DIR);
        $uploader->addAllowedType('csv');
        $uploader->save();
        if ($uploader->hasRefusedFiles()) {
            $this->redirect()->setMessage('Nahrávané soubory musí být typu CSV.');
        }
        foreach ($uploader->getSavedFiles() as $path) {
            $this->processCsv($path);
            $this->redirect()->setMessage('Soubor ' . str_replace(self::TEMP_DIR, '', $path) . ' byl zpracován.');
        }
    }
}
