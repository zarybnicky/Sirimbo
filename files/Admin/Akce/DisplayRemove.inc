<?php
if(!is_array(get('u')))
	notice('Žádné akce');
?>
<form action="" method="POST">
Opravdu chcete odstranit akce:<br/><br/>
<?php
foreach(get('u') as $id) {
	$data = DBAkce::getSingleAkce($id);
	if(empty($data))
		continue;
	echo $data['a_jmeno'], '<br />',
		'<input type="hidden" name="akce[]" value="', $id, '" />';
}
?>
<br/>
<?php echo getReturnInput()?>
<button type="submit" name="action" value="confirm">Odstranit</button>
<a href="<?php echo Request::getReferer()?>">Zpět</a>
</form>