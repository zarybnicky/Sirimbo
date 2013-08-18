<?php
header_main('Správa aktualit');
notice($this->redirect()->getRedirectMessage());

if(!is_array(get('u')))
	notice('Žádné aktuality');
?>
<form action="<?php echo Request::getURI();?>" method="POST">
Opravdu chcete odstranit články:<br/><br/>
<?php
foreach(get('u') as $id) {
	$data = DBAktuality::getSingleAktualita($id);
	echo $data['at_jmeno'], '<br />';
	echo '<input type="hidden" name=aktuality[]" value="', $id, '" />';
}
?>
<br/>
<?php echo getReturnInput()?>
<button type="submit" name="action" value="confirm">Odstranit</button>
<a href="<?php echo Request::getReferer()?>">Zpět</a>
</form>