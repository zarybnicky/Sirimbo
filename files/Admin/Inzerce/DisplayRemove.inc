<?php
header_main('Správa inzerce');
notice(View::getRedirectMessage());

if(!is_array(get('u')))
	notice('Žádné inzeráty');
?>
<form action="<?php echo Request::getURI();?>" method="POST">
Opravdu chcete odstranit inzeráty:<br/><br/>
<?php
foreach(get('u') as $id) {
	$data = DBInzerce::getSingleInzerat($id);
	echo $data['i_nadpis'], '<br />';
	echo '<input type="hidden" name=inzerce[]" value="', $id, '" />';
}
?>
<br/>
<?php echo getReturnInput()?>
<button type="submit" name="action" value="confirm">Odstranit</button>
<a href="<?php echo Request::getReferer()?>">Zpět</a>
</form>