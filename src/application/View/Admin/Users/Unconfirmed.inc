<h1>Správa uživatelů</h1>
<h2>Nepotvrzení uživatelé</h2>
<?php notice($this->redirect()->getMessage());?>
<form action="" method="POST">
<?php
echo (new TKOlomouc\View\Helper\Menu())
    ->content('Potvrdit', 'confirm', true)
    ->content('Odstranit', 'remove', true)
    ->content('Zpět', '../users');

echo (new TKOlomouc\View\Helper\Table())
    ->column('checkBox', '')
    ->column('group', 'Úroveň')
    ->column('skupina', 'Skupina')
    ->column('dancer', 'Aktivní<br/>tanečník', 'center')
    ->column('fullName', 'Jméno', '', 'max-width:140px')
    ->column('narozeni', 'Datum<br/>narození')
    ->data($this->data);
?>
</form>