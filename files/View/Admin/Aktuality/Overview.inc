<?= $this->header('Správa aktualit') ?>
<div class="container">
    <?php
    if ($this->showMenu) {
        echo '<form action="" method="get">';

        echo $this->select('f')
                  ->set($this->f ?: '0')
                  ->option('0', 'vše')
                  ->option(AKTUALITY_CLANKY, 'články')
                  ->option(AKTUALITY_KRATKE, 'krátké zprávy');
        echo $this->submit('Zobrazit');
        echo '</form><br/>';
    }
    ?>
    <form action="" method="POST">
        <?php
        if ($this->showMenu) {
            echo $this->menu()
                      ->content('Přidat', 'aktuality/add')
                      ->content('Odebrat', 'remove', true);
        }
        ?>
        <div style="margin-right:150px">
            <?= $this->table()
                     ->style('width:100%')
                     ->data($this->data)
                     ->column('checkBox', '')
                     ->column('name', 'Jméno')
                     ->column('category', 'Kategorie', null, 'width:120px')
                     ->column('links', 'Upravit', null, 'width:120px');
            ?>
        </div>
    </form>
</div>
