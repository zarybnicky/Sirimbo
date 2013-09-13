<?php
function _getItem($name, $href) {
	$href = 'admin/' . $href;
	if(strripos(Request::getLiteralURL(), $href) !== (strlen(Request::getLiteralURL()) - strlen($href)))
		echo '<a href="/' . $href . '">' . $name . '</a>';
	else
		echo $name;
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
	<li><?php _getItem('Přehled', 'platby');?></li>
	<li><?php _getItem('Kategorie', 'platby/structure');?></li>
	<li><?php _getItem('Hledání', 'platby/items');?></li>
	<li><?php _getItem('Import', 'platby/raw');?></li>
	<li><?php _getItem('Nezařazené platby', 'platby/manual');?></li>
	<li><?php _getItem('Vyřazené platby', 'platby/discarded');?></li>
</ul>
<?php notice($this->redirect()->getMessage());?>