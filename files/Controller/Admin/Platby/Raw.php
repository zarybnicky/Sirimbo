<?php
include_once('files/Controller/Admin/Platby.php');
class Controller_Admin_Platby_Raw extends Controller_Admin_Platby {
	const TEMP_DIR = './upload/csv/';
	
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		if(!empty($_POST) && post('action') == 'upload') {
			$this->processUpload();
		}
		$workDir = new DirectoryIterator(self::TEMP_DIR);
		$workDir->rewind();
		foreach($workDir as $fileInfo) {
			if(!$fileInfo->isFile())
				continue;
			$this->processCsv($fileInfo->getPathname());
			$this->redirect()->setMessage('Soubor ' . $fileInfo->getFilename() . ' byl zpracován.');
		}
		
		$this->render('files/View/Admin/Platby/RawUpload.inc');
	}
	function select_columns($id = null) {
		$path = self::TEMP_DIR . str_replace('../', '', get('path'));
		
		if(!empty($_POST)) {
			$this->processCsv($path, array(
				'specific' => post('specific'),
				'variable' => post('variable'),
				'date' => post('date'),
				'amount' => post('amount')
			));
			$this->redirect('/admin/platby/raw', 'Soubor ' . get('path') . ' byl zpracován.');
		}
		$parser = $this->getParser($path);
		$this->recognizeHeaders($parser->headers(), $specific, $variable, $date, $amount);
		
		$data = array();
		foreach($parser->headers() as $key => $name) {
			$data[] = array(
					'column' => $name,
					'specific' => getRadio('specific', $name, $name == $specific),
					'variable' => getRadio('variable', $name, $name == $variable),
					'date' => getRadio('date', $name, $name == $date),
					'amount' => getRadio('amount', $name, $name == $amount)
			);
		}
		$this->render('files/View/Admin/Platby/RawColumnSelect.inc', array(
				'data' => $data
		));
	}
	private function getParser($path) {
		$fileinfo = new SplFileInfo($path);
		if (!$fileinfo->isReadable())
			$this->redirect('/admin/platby/raw', 'Soubor ' . str_replace(self::TEMP_DIR, '', $path) . ' není přístupný.');
		$parser = new CSVParser($fileinfo->openFile('r'));
		$parser->associative(true);
		return $parser;
	}
	private function processCsv($parser, $columns = null) {
		if(!is_a($parser, 'CSVParser')) {
			if(!is_string((string) $parser))
				throw new InvalidArgumentException('$parser (' . gettype($parser) . ') is not a CSVParser or a string');
			$parser = $this->getParser($parser);
		}
		$headers = $parser->headers();
		if($columns === null) {
			$this->recognizeHeaders($headers, $specific, $variable, $date, $amount);
		} else {
			$specific = $columns['specific'];
			$variable = $columns['variable'];
			$date = $columns['date'];
			$amount = $columns['amount'];
		}
		if(!$this->checkHeaders($headers, $specific, $variable, $date, $amount)) {
			$this->redirect('/admin/platby/raw/select_columns?path=' . str_replace(self::TEMP_DIR, '', $path),
					'Skript nemohl rozpoznat sloupce nutné pro zařazení plateb, je potřeba udělat to ručně. (soubor: ' . $path . ')');
		}
		$userLookup = $this->getUserLookup(false);
		$categoryLookup = $this->getCategoryLookup(true, true, false);
		
		foreach($parser as $array) {
			if(!$array)
				continue;
			$serialized = serialize($array);
			$hash = md5($serialized);
			
			list($dataSpecific, $dataVariable, $dataDate, $dataAmount, $dataPrefix) =
				$this->formatData($array[$specific], $array[$variable], $array[$date], $array[$amount]);
			
			if(!isset($userLookup[$dataVariable]) || !isset($categoryLookup[$dataSpecific])) {
				DBPlatbyRaw::insert($serialized, $hash, '0', '0', false);
				continue;
			} else {
				$dataSpecific = $categoryLookup[$dataSpecific]['pc_id'];
				
				DBPlatbyRaw::insert($serialized, $hash, '1', '0', true);
				DBPlatbyItem::insert($dataVariable, $dataSpecific, DBPlatbyRaw::getInsertId(), $dataAmount, $dataDate, $dataPrefix);
			}
		}
		unlink($parser->getFileObject()->getRealPath());
	}
	private function processUpload() {
		$upload = new UploadHelper();
		$upload->upload('in')->loadFromPost();
		
		$validFiles = $upload->hasValidFiles();
		if($upload->hasInvalidFiles()) {
			$this->redirect()->setMessage($upload->getErrorMessages());
			return;
		} elseif($upload->hasEmptyFiles() && empty($validFiles)) {
			$this->redirect()->setMessage('Vyberte prosím nějaký soubor (prázdné soubory jsou automaticky odmítnuty).');
			return;
		}
		$uploader = $upload->getFilledUploader();
		$uploader->setOutputDir(self::TEMP_DIR);
		$uploader->addAllowedType('csv');
		$uploader->save();
		if($uploader->hasRefusedFiles()) {
			$this->redirect()->setMessage('Nahrávané soubory musí být typu CSV.');
		}
		foreach($uploader->getSavedFiles() as $path) {
			$this->processCsv($path);
			$this->redirect()->setMessage('Soubor ' . str_replace(self::TEMP_DIR, '', $path) . ' byl zpracován.');
		}
	}
}