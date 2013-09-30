<?php
class Controller_Utility_Urldecode extends Controller_Abstract {
	function view($id = null) {
		echo '<form action="" method="GET">';
		echo '<h1>URL Decode</h1>';
		echo 'Text pro urldecode(): <input type="text" name="t" /><br/>';
		echo '<input type="submit" />';
		echo '</form>';
	
		if(get('t'))
			echo urldecode(get('t'));
	}
}
?>