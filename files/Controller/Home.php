<?php
class Controller_Home extends Controller_Abstract {
	function view($id = null) {
		if(NABOR && $_SERVER['REQUEST_URI'] == '/')
			View::redirect('/nabor');
		
		notice(View::getRedirectMessage());

		echo '<style>#content {margin-left:110px !important;</style>';
		echo '<div style="width:710px;">';
		
		echo Helper::get()->clanky()->number(5)->slideBox(true);
		echo '<br/>';
		echo '<div style="float:right;">';
		echo '<div style="text-align:left;margin: 0 auto;">';
		echo Helper::get()->zpravy()->number(20);
		echo '</div><br/><br/>';
		echo '<span class="little">Podporuje nás:</span>';
		echo '<div style="text-align:center;"><a href="http://www.kr-olomoucky.cz/" target="_blank"><img alt="" src="/style/kraj-logo.gif" /></a></div>';
		echo '<div style="text-align:center;"><a href="http://www.olomouc.eu/" target="_blank"><img alt="" src="/style/olomouc-logo.jpg" /></a></div>';
		echo '<br/>';
		echo '<span class="little" style="line-height:200%;">Mediální partner:<br/></span>';
		echo '<iframe src="http://poutaky.denik.cz/olomoucky.denik.cz-300x300-zpravy_region.html"
			width="300" height="300" border="0" frameborder="0" scrolling="no"></iframe>';
		echo '</div>';
		echo '<div style="margin-right:305px;">';
		echo Helper::get()->clanky()->number(9)->offset(5)->slideBox(false);
		echo '</div>';
		
		echo '</div>';
	}
}
?>