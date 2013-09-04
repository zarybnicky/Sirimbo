<?php
function _getItem($name, $href) {
	if(strripos(Request::getLiteralURL(), "admin/$href") !== false)
		echo $name;
	else
		echo '<a href="/admin/' . $href . '">' . $name . '</a>';
}
header_main('Správa plateb');
if(TISK)
	return;
?>
<style>
#adminNavigation li {
	display:inline;
	font-size: 115%;
	color: #572E00;
	padding: 0 5px;
	font-weight:bold;
} #adminNavigation li a {
	font-weight:normal;
}
</style>
<ul id="adminNavigation">
	<li><?php _getItem('Přehled', 'platby/view');?></li>
	<li><?php _getItem('Kategorie', 'platby/category');?></li>
	<li><?php _getItem('Hledání', 'platby/items');?></li>
	<li><?php _getItem('Import', 'platby/raw');?></li>
	<li><?php _getItem('Nezařazené platby', 'platby/manual');?></li>
	<li><?php _getItem('Vyřazené platby', 'platby/discarded');?></li>
</ul>