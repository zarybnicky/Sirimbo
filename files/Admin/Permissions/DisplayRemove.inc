<?php
if(!is_array(get('u')))
	notice('Žádné platby');
?>
<form action="<?php echo Request::getURI();?>" method="POST">
<?php notice('Bude nutné přiřadit uživatelům z těchto skupin jinou skupinu!'); ?>
Opravdu chcete odstranit uživatelské úrovně:<br/><br/>
<?php
foreach(get('u') as $id) {
	$data = DBPermissions::getSingleGroup($id);
	echo $data['pe_name'], '<br />';
	echo '<input type="hidden" name="permissions[]" value="', $id, '" />';
}
?>
<br/>
<?php echo getReturnInput()?>
<button type="submit" name="action" value="confirm">Odstranit</button>
<a href="<?php echo Request::getReferer()?>">Zpět</a>
</form>