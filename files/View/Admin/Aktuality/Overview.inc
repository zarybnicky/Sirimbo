<?= $this->header('Správa aktualit') ?>
<div class="container full">
    <?php
    echo $this->notice($this->redirect()->getMessage());

    if ($this->showMenu) {
        echo '<form action="" method="get">';

        echo $this->select()
                  ->get()
                  ->name('f')
                  ->value('all')
                  ->option('0', 'vše')
                  ->option(AKTUALITY_CLANKY, 'články')
                  ->option(AKTUALITY_KRATKE, 'krátké zprávy')
                  ->option(AKTUALITY_VIDEA, 'videa');
        echo '<button type="submit">Zobrazit</button>';
        echo '</form><br/>';
    }
    ?>
    <form action="" method="POST">
        <?php
        if ($this->showMenu) {
            echo $this->menu()
                      ->float(MenuHelper::FLOAT_RIGHT)
                      ->content('Přidat', 'aktuality/add')
                      ->content('Upravit', 'edit', true)
                      ->content('Upravit galerii', 'foto', true)
                      ->content('Odebrat', 'remove', true);
        }
        ?>
        <div style="margin-right:150px">
            <?php
            echo $this->table()
                      ->style('width:100%')
                      ->data($this->data)
                      ->column('checkBox', '')
                      ->column('name', 'Jméno')
                      ->column('category', 'Kategorie', null, 'width:120px');
            ?>
        </div>
    </form>
</div>
