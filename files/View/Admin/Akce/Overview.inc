<?= $this->header('Správa akcí') ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()) ?>
    <form action="" method="POST">
        <?php
if (!TISK) {
    echo $this->menu()
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
          ->column('links', 'Upravit')
          ->data($this->data);
echo '</div>';
        ?>
    </form>
</div>
