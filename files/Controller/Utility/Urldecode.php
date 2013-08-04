<?php
class Controller_Utility_Urldecode extends Controller_Abstract {
	function view($id = null) {
		echo '<form action="', Request::getURI(), '" method="GET">';
		header_main('URL Decode');
		echo 'Text pro urldecode(): <input type="text" name="t" /><br/>';
		echo '<input type="submit" />';
		echo '</form>';
	
		if(get('t'))
			echo urldecode(get('t'));
	}
}
?>