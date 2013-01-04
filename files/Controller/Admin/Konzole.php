<?php
class Controller_Admin_Konzole implements Controller_Interface {
	function view($id = null) {
		if(!empty($_POST) && post('code')) {
			$r = eval(stripslashes(post('code')));
			if($r === FALSE)
				notice('Kód obsahuje syntaktickou chybu');
			elseif(!empty($r))
				notice(dumpVar($r));
			else
				notice('Success!');
		}
		echo '<form action="' .  $_SERVER['REQUEST_URI'] . '" method="post">';
		echo 'Kód:<br/>';
		echo '<textarea name="code" rows="10" cols="20"></textarea><br/>';
		echo '<button type="submit">Zpracovat</button>';
		echo '</form>';
	}
}
?>