<?php
class View {
	public static function viewString($l) {
		include(TISK ? HEADER_TISK : HEADER);
		echo $l;
		include(TISK ? FOOTER_TISK : FOOTER);
		
		exit;
	}

	public static function viewStatic($l) {
		if(isset(Settings::$no_headers[$l])) {
			if(file_exists($l)) {
				$fd = fopen($l, 'r');
				echo fread($fd, filesize($l));
			} else {
				echo "Stránka \"$l\" nenalezena.";
			}
			exit;
		}
		
		ob_start();
		
		include(TISK ? HEADER_TISK : HEADER);
		if(file_exists($l)) {
			$fd = fopen($l, 'r');
			echo fread($fd, filesize($l));
		}
		else {
			echo "Stránka \"$l\" nenalezena.";
		}
		include(TISK ? FOOTER_TISK : FOOTER);
		
		ob_end_flush();
		
		exit;
	}

	public static function viewDynamic($l) {
		if(isset(Settings::$no_headers[$l])) {
			if(file_exists($l)) {
				include($l);
			} else {
				echo "Stránka \"$l\" nenalezena.";
			}
			exit;
		}
		//if in_array($l, $Cache->getInstance()) && $Cache->isUptoDate($l))
		//	echo $Cache->get($l);
		//	exit;
		//else
		//	$fh = fopen($myFile, 'r');
		//	$theData = fread($fh, filesize($myFile));
		//	fclose($fh);
		ob_start();
		
		ob_start();
		if(file_exists($l)) {
			include($l);
		} else {
			echo "Stránka \"$l\" nenalezena.";
		}
		$main = ob_get_clean();
		//TODO: Caching
		
		include(TISK ? HEADER_TISK : HEADER);
		echo $main;
		include(TISK ? FOOTER_TISK : FOOTER);
		
		ob_end_flush();
		exit;
	}
	
	public static function viewError($er_id) {
		header("Location: /error?id=$er_id");
		
		if($er_id == ER_DATABASE || $er_id == ER_DATABASE_CONNECTION)
			Log::write('MySQL Error: ' . mysql_errno() . ': ' . mysql_error());
		elseif($er_id == ER_AUTHORIZATION || $er_id == ER_SCRIPT_FATAL)
			exit;
		else
			Log::write("Error: $er_id");
		exit;
	}
	
	public static function redirect($link, $message = '', $replaceMessage = false) {
		header('Location: ' . $link);
		
		if(!$message)
			exit;
		
		if(isset($_SESSION['REDIRECT_MESSAGE']) && $replaceMessage == false)
			$_SESSION['REDIRECT_MESSAGE'] .= '<br />' . $message;
		else
			$_SESSION['REDIRECT_MESSAGE'] = $message;
		exit;
	}
	
	public static function setRedirectMessage($message, $replace = false) {
		if(isset($_SESSION['REDIRECT_MESSAGE']) && $replace == false)
			$_SESSION['REDIRECT_MESSAGE'] .= '<br />' . $message;
		else
			$_SESSION['REDIRECT_MESSAGE'] = $message;
	}
	
	public static function getRedirectMessage() {
		if(isset($_SESSION['REDIRECT_MESSAGE'])) {
			$message = $_SESSION['REDIRECT_MESSAGE'];
			unset($_SESSION['REDIRECT_MESSAGE']);
		} else {
			$message = '';
		}
		return $message;
	}
}
?>
