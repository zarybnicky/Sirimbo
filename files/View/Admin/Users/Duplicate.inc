<?php
header_main('Správa uživatelů');
header_minor('Duplicitní uživatelé');
notice($this->redirect()->getRedirectMessage());
?>
<form action="" method="POST">
<?php
echo $this->menu()
	->float(MenuHelper::FLOAT_RIGHT)
	->content('Odstranit', 'remove', true)
	->content('Zpět', '../users');
?>
<?php
echo $this->table()
	->column('checkBox', '')
	->column('colorBox', '')
	->column('fullName', 'Jméno', '', 'max-width:140px;')
	->column('email', 'Email')
	->column('telefon', 'Telefon')
	->column('narozeni', 'Datum<br/>narození')
	->column('timestamp', 'Poslední<br/>úprava')
	->data($this->data);
?>
</form>