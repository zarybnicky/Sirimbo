<?= $this->header('Správa oprávnění') ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()) ?>
    <form action="" method="POST">
        <?php
        if ($this->showMenu) {
            echo $this->menu()
                      ->content('Přidat', 'permissions/add')
                      ->content('Upravit', 'edit', true)
                      ->content('Odebrat', 'remove', true);
        }
        ?>
        <div<?= $this->showMenu ? 'style="margin-right:150px"' : '' ?>>
            <?= $this->table()
                     ->style('width:100%')
                     ->data($this->data)
                     ->column('checkBox', '')
                     ->column('name', 'Jméno')
                     ->column('description', 'Popis') ?>
        </div>
    </form>
</div>
