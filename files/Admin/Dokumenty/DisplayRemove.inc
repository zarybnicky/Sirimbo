<?php
if(!is_array(get('u')))
	notice('Žádné dokumenty');
?>
<form action="<?php echo Request::getURI();?>" method="POST">
Opravdu chcete odstranit dokumenty:<br/><br/>
<?php
foreach(get('u') as $id) {
	$name = DBDokumenty::getDokumentName($id);
	echo $name, '<br />';
	echo '<input type="hidden" name="dokumenty[]" value="', $id, '" />';
}
?>
<br/>
<?php echo getReturnInput()?>
<button type="submit" name="action" value="confirm">Odstranit</button>
<a href="<?php echo Request::getReferer()?>">Zpět</a>
</form>