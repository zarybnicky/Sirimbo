<?= $this->header('Správa aktualit') ?>
<div class="container">
    <?php
    if ($this->showMenu) {
        echo '<form action="" class="form-inline" method="get">';

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
        <a href="/admin/aktuality/add" class="btn btn-primary">Nový článek</a>
        <?= $this->table()
             ->style('width:100%')
             ->data($this->data)
             ->column('buttons', '')
             ->column('links', 'Upravit')
             ->column('name', 'Jméno')
             ->column('category', 'Kategorie')
             ->column('added', 'Přidán dne') ?>
    </form>
</div>
