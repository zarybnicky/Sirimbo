<?php
if(!is_array(get('u')))
	notice('Žádné platby');
?>
<form action="<?php echo Request::getURI();?>" method="POST">
Opravdu chcete odstranit platby:<br/><br/>
<?php
foreach(get('u') as $id) {
	$data = DBPlatby::getSinglePlatba($id);
	echo 'od "', $data['u_prijmeni'], ', ', $data['u_jmeno'],
		'" - placeno ', formatDate($data['up_placeno']), '<br />';
	echo '<input type="hidden" name="platby[]" value="', $id, '" />';
}
?>
<br/>
<?php echo getReturnInput()?>
<button type="submit" name="action" value="remove_confirm">Odstranit</button>
<a href="<?php echo Request::getReferer()?>">Zpět</a>
</form>