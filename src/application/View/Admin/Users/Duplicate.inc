<h1>Správa uživatelů</h1>
<h2>Duplicitní uživatelé</h2>
<?php notice($this->redirect()->getMessage()); ?>
<form action="" method="POST">
<?php
echo $this->menu()
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