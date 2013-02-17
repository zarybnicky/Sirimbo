<?php
class Controller_Home implements Controller_Interface {
	function view($id = null) {
		if(NABOR && $_SERVER['REQUEST_URI'] == '/')
			View::redirect('/nabor');
		
		notice(View::getRedirectMessage());
		
		echo '<div>';
		echo Helper::get()->clanky()->number(5)->slideBox(true);
		echo '<br/>';
		echo '<div style="float:right;">';
		echo Helper::get()->zpravy()->number(20);
		echo '</div>';
		echo '<div style="margin-right:265px;">';
		echo Helper::get()->clanky()->number(20)->offset(5)->slideBox(false);
		echo '</div>';
		
		echo '</div>';
	}
}
?>