<?php
header_main('Správa anket');
notice($this->redirect()->getRedirectMessage());

if(!is_array(get('u')))
	notice('Žádné akce');
?>
<form action="<?php echo Request::getURI();?>" method="POST">
Opravdu chcete odstranit ankety:<br/><br/>
<?php
foreach(get('u') as $id) {
	$data = DBAnkety::getSingleAnketa($id);
	if(empty($data))
		continue;
	echo $data['ak_jmeno'], '<br />',
		'<input type="hidden" name=ankety[]" value="', $id, '" />';
}
?>
<br/>
<?php echo getReturnInput()?>
<button type="submit" name="action" value="remove_confirm">Odstranit</button>
<a href="<?php echo Request::getReferer()?>">Zpět</a>
</form>