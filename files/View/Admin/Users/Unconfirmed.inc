<?php
header_main('Správa uživatelů');
header_minor('Nepotvrzení uživatelé');
notice($this->redirect()->getRedirectMessage());
?>
<form action="" method="POST">
<?php
echo $this->menu()
	->float(MenuHelper::FLOAT_RIGHT)
	->content('Potvrdit', 'confirm', true)
	->content('Odstranit', 'remove', true)
	->content('Zpět', '../users');

echo $this->table()
	->column('checkBox', '')
	->column('group', 'Úroveň')
	->column('skupina', 'Skupina')
	->column('dancer', 'Aktivní<br/>tanečník', 'center')
	->column('fullName', 'Jméno', '', 'max-width:140px')
	->column('narozeni', 'Datum<br/>narození')
	->data($this->data);
?>
</form>