<?php
include_once('files/Controller/Admin/Platby.php');
class Controller_Admin_Platby_Raw extends Controller_Admin_Platby {
	const UPLOAD_DIR = './upload/csv/';
	
	function __construct() {
		Permissions::checkError('platby', P_OWNED);
	}
	function view($id = null) {
		if(!empty($_POST)) {
			switch(post('action')) {
				case 'upload':
					$this->processUpload();
					break;
			}
		}
		$workDir = new DirectoryIterator(self::UPLOAD_DIR);
		$workDir->rewind();
		foreach($workDir as $fileInfo) {
			if(!$fileInfo->isFile())
				continue;
			$this->processCsv($fileInfo->getPathname());
			$this->redirect()->setRedirectMessage('Soubor ' . $fileInfo->getFilename() . ' byl zpracován.');
		}
		
		$this->render('files/View/Admin/Platby/RawUpload.inc');
	}
	function select_columns($id = null) {
		$path = self::UPLOAD_DIR . str_replace('../', '', get('path'));
		
		if(!empty($_POST)) {
			$this->processCsv($path, array(
				'specific' => post('specific'),
				'variable' => post('variable'),
				'date' => post('date'),
				'amount' => post('amount')
			));
			$this->redirect('/admin/platby/raw', 'Soubor ' . get('path') . ' byl zpracován.');
		}
		$fileinfo = new SplFileInfo($path);
		if (!$fileinfo->isReadable())
			$this->redirect('/admin/platby/raw', 'Soubor ' . get('path') . ' není přístupný.');
		$parser = new CSVParser($fileinfo->openFile('r'));
		$parser->associative(true);
		
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
	function recognizeHeaders($headers, &$specific, &$variable, &$date, &$amount) {
		foreach($headers as $key => $value) {
			if($specific === null && mb_stripos($value, 'specif') !== false)
				$specific = $value;
			if($variable === null && mb_stripos($value, 'variab') !== false)
				$variable = $value;
			if($date === null && mb_stripos($value, 'datum') !== false)
				$date = $value;
			if($amount === null && mb_stripos($value, 'částka') !== false)
				$amount = $value;
		}
	}
	function checkHeaders($headers, &$specific, &$variable, &$date, &$amount, $columns) {
		$specific = $columns['specific'];
		$variable = $columns['variable'];
		$date = $columns['date'];
		$amount = $columns['amount'];
		if(array_search($specific, $headers) === false)
			$specific = null;
		if(array_search($variable, $headers) === false)
			$variable = null;
		if(array_search($date, $headers) === false)
			$date = null;
		if(array_search($amount, $headers) === false)
			$amount = null;
	}
	function processCsv($path, $columns = null) {
		$fileinfo = new SplFileInfo($path);
		if (!$fileinfo->isReadable())
			$this->redirect('/admin/platby/raw', 'Soubor ' . $path . ' není přístupný.');
		$parser = new CSVParser($fileinfo->openFile('r'));
		$parser->associative(true);
		
		$headers = $parser->headers();
		if($columns === null) {
			$this->recognizeHeaders($headers, $specific, $variable, $date, $amount);
		} else {
			$this->checkHeaders($headers, $specific, $variable, $date, $amount, $columns);
		}
		if($specific === null || $variable === null || $date === null || $amount === null) {
			$this->redirect('/admin/platby/raw/select_columns?path=' . str_replace(self::UPLOAD_DIR, '', $path),
					'Skript nemohl rozpoznat sloupce nutné pro zařazení plateb, je potřeba udělat to ručně. (soubor: ' . $path . ')');
		}
		
		$userLookup = DBUser::getUsersLookup();
		$categoryLookup = DBPlatbyCategory::getCategoryLookup();
		
		foreach($parser as $array) {
			if(!$array)
				continue;
			$serialized = serialize($array);
			$hash = md5($serialized);
			
			if(!isset($userLookup[(int) $array[$variable]]) || !isset($categoryLookup[(int) $array[$specific]])) {
				DBPlatbyRaw::insert($serialized, $hash, '0', '0', false);
				continue;
			}
			$dataVariable = (int) $array[$variable];
			$dataSpecific = $categoryLookup[(int) $array[$specific]]['pc_id'];
			$dataDate = (string) new Date($array[$date]);
			$dataAmount = (int) $array[$amount];
			
			DBPlatbyRaw::insert($serialized, $hash, '1', '0', true);
			$dataIdRaw = DBPlatbyRaw::getInsertId();
			DBPlatbyItem::insert($dataVariable, $dataSpecific, $dataIdRaw, $dataAmount, $dataDate);
		}
		unlink($path);
	}
	function processUpload() {
		$upload = new UploadHelper();
		$upload->upload('in')->loadFromPost();
		
		if($upload->hasInvalidFiles()) {
			$this->redirect()->setRedirectMessage($upload->getErrorMessages());
			return;
		} elseif($upload->hasEmptyFiles() && empty($upload->hasValidFiles())) {
			$this->redirect()->setRedirectMessage('Vyberte prosím nějaký soubor (prázdné soubory jsou automaticky odmítnuty).');
			return;
		}
		$uploader = $upload->getFilledUploader();
		$uploader->setOutputDir(self::UPLOAD_DIR);
		$uploader->addAllowedType('csv');
		$uploader->save();
		if($uploader->hasRefusedFiles()) {
			$this->redirect()->setRedirectMessage('Nahrávané soubory musí být typu CSV.');
		}
		foreach($uploader->getSavedFiles() as $path) {
			$this->processCsv($path);
			$this->redirect()->setRedirectMessage('Soubor ' . str_replace(self::UPLOAD_DIR, '', $path) . ' byl zpracován.');
		}
	}
}