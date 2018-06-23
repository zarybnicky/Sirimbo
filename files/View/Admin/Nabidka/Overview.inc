<?= $this->header('Správa nabídky') ?>
<div class="container full">
    <form action="" method="POST">
        <?php
        if ($this->showMenu) {
            echo $this->menu()
                      ->content('Přidat', 'nabidka/add')
                      ->content('Duplikovat', 'duplicate', true)
                      ->content('Odstranit', 'remove', true)
                      ->content('Uložit změny', 'save', true);
        }
        ?>
        <div style="margin-right:150px;">
        <?= $this->table()
                 ->style('width:100%;')
                 ->data($this->data)
                 ->column('checkBox', '')
                 ->column('fullName', 'Trenér')
                 ->column('date', 'Datum')
                 ->column('visible', 'Viditelný')
                 ->column('links', 'Upravit'); ?>
        </div>
    </form>
</div>
