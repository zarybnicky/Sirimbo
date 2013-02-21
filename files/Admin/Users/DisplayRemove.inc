<?php
header_main('Správa uživatelů');
notice(View::getRedirectMessage());

if(!is_array(get('u')))
	notice('Žádní uživatelé');
?>
<form action="<?php echo Request::getURI();?>" method="POST">
Opravdu chcete odstranit uživatele:<br/><br/>
<?php
foreach(get('u') as $id) {
	$data = DBUser::getUserData($id);
	if(empty($data))
		continue;
	echo $data['u_prijmeni'], ', ', $data['u_jmeno'], ' (', $data['u_login'], ')',
		'<input type="hidden" name="users[]" value="', $id, '" /><br />';
}
?>
<br/>
<?php echo getReturnInput()?>
<button type="submit" name="action" value="confirm">Odstranit</button>
<a href="<?php echo Request::getReferer()?>">Zpět</a>
</form>