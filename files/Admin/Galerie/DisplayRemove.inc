<?php
if(!is_array(get('u')))
	notice('Žádné složky');
?>
<form action="<?php echo Request::getURI();?>" method="POST">
Opravdu chcete odstranit složky a jejich podsložky:<br/><br/>
<?php
foreach(post('u') as $id) {
	$data = DBGalerie::getSingleDir($id);
	echo $data['gd_name'], '<br />';
	echo '<input type="hidden" name="galerie[]" value="', $id, '" />';
}
?>
<br/>
<?php echo getReturnInput()?>
<button type="submit" name="action" value="remove_confirm">Odstranit</button>
<a href="<?php echo Request::getReferer()?>">Zpět</a>
</form>