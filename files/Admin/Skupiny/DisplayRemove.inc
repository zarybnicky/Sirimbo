<?php
if(!is_array(get('u')))
	notice('Žádné skupiny k vymazání');
?>
<form action="<?php echo Request::getURI();?>" method="POST">
Opravdu chcete odstranit skupiny:<br/><br/>
<?php
foreach(get('u') as $id) {
	$data = DBSkupiny::getSingleSkupina($id);
	if(empty($data))
		continue;
	echo $data['us_popis'],
		'<input type="hidden" name="skupiny[]" value="', $id, '" /><br />';
}
?>
<br/>
<?php echo getReturnInput()?>
<button type="submit" name="action" value="remove_confirm">Odstranit</button>
<a href="<?php echo Request::getReferer()?>">Zpět</a>
</form>