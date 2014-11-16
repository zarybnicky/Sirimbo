<?= $this->header('Správa akcí') ?>
<div class="container full">
<form action="" method="POST">
<?php
echo $this->notice($this->redirect()->getMessage());
if (!TISK) {
    echo $this->menu()
              ->float(MenuHelper::FLOAT_RIGHT)
              ->content('Přidat', 'akce/add')
              ->content('Odstranit', 'remove', true)
              ->content('Uložit změny', 'save', true);
    echo '<div style="margin-right:150px;">';
} else {
    echo '<div>';
}
echo $this->table()
          ->style('width:100%')
          ->column('checkBox', '')
          ->column('name', 'Jméno')
          ->column('date', 'Datum')
          ->column('userCount', 'Kapacita')
          ->column('visible', 'Viditelný')
          ->column('links', 'Odkazy')
          ->data($this->data);
echo '</div>';
?>
</form>
</div>
