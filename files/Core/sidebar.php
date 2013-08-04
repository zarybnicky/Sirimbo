<?php
class Sidebar {
	function menuHeader() {
		return $this->blackBox('<span class="logo"></span>Menu');
	}
	function menuItem($text, $link, $module = '', $permission = L_ALL) {
		if($module != '' && !Permissions::check($module, $permission))
			return;
		
		$active = stripos(Request::getURI(), $link) === 0;
		$r = '<a href="' . $link . '"><span class="arrow">.</span>' . $text . '</a>';
		if($active)
			return $this->blackBox($r . '<span class="point">.</span>', 'sidebar current');
		else
			return $this->whiteBox($r, 'sidebar');
	}
	function blackBox($text, $class = '') {
		return '<div class="dark-out' .
			($class ? ' ' . $class : '') . '"><div class="dark-in">' . $text . '</div></div>';
	}
	function whiteBox($text, $class = '') {
		return '<div class="light-out' .
			($class ? ' ' . $class : '') . '"><div class="light-in">' . $text . '</div></div>';
	}
}
?>