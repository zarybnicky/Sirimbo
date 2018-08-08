<?= $this->header('Správa rozpisů') ?>

<div class="container">
  <form action="" method="POST">
    <?php
    if ($this->showMenu) {
        echo $this->menu()
                 ->content('Přidat', 'rozpis/add')
                 ->content('Duplikovat', 'duplicate', true)
                 ->content('Odstranit', 'remove', true)
                 ->content('Uložit změny', 'save', true);
    }
    ?>
    <div style="margin-right:150px;">
      <?= $this->table()
             ->style('width:100%')
             ->column('checkBox', '')
             ->column('fullName', 'Trenér')
             ->column('datum', 'Datum')
             ->column('kde', 'Místo')
             ->column('visible', 'Viditelný')
             ->column('links', 'Upravit')
             ->data($this->data)
      ?>
    </div>
  </form>
</div>
