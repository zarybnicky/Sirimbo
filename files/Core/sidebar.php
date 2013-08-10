<?php
class Sidebar {
	public static $hasSidebarDiv = false;
	
	public function __construct() {
		if(!Sidebar::$hasSidebarDiv)
			echo '<style type="text/css">#content{margin-left:110px !important;}</style>';
		Sidebar::$hasSidebarDiv = true;
	}
	
	function menuHeader() {
		return $this->blackBox('<span class="logo"></span>Menu');
	}
	function menuItem($text, $link, $module = '', $permission = L_ALL) {
		if($module != '' && !Permissions::check($module, $permission))
			return;
		
		$active = stripos(Request::getURI(), $link) === 0;
		$r = '<span class="arrow">.</span>' . $text;
		if($active)
			return $this->blackBox($r . '<span class="point">.</span>', 'sidebar current', $link);
		else
			return $this->whiteBox($r, 'sidebar', $link);
	}
	function blackBox($text, $class = '', $link = '') {
		$text = '<div class="dark-in">' . $text . '</div>';
		if($link != '') {
			$text = '<a href="' . $link . '">' . $text . '</a>';
		}
		return '<div class="dark-out' .
			($class ? ' ' . $class : '') . '">' . $text . '</div>';
	}
	function whiteBox($text, $class = '', $link = '') {
		$text = '<div class="light-in">' . $text . '</div>';
		if($link != '') {
			$text = '<a href="' . $link . '">' . $text . '</a>';
		}
		return '<div class="light-out' .
			($class ? ' ' . $class : '') . '">' . $text . '</div>';
	}
	function commonItems() {
		if(!Database::isDatabaseError() && stripos(Request::getURI(), '/home') === false) {
			ob_start();
			$result = DisplayAnkety::viewAnkety(true, true, getIP());
			$ankety = ob_get_clean();
				
			if($result) {
				echo $this->blackbox('<span class="logo"></span>Ankety');
				echo $this->whiteBox($ankety);
			}
			echo '<div><a href="https://www.facebook.com/groups/44213111683/" target="_blank">';
			echo '<img alt="" src="/style/fb-logo.png" />';
			echo '</a></div>';
		}
		/*
		<div class="dark-out"><div class="dark-in">
			<span class="logo"></span>Sponzoři
		</div></div>
		<div class="light-out"><div class="light-in">	
			???
		</div></div>
		*/
	}
}
?>