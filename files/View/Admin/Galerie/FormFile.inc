<?= $this->header('Správa fotogalerie', 'Upravit soubor') ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()); ?>
    <form action="" method="post">
        <table>
            <tr>
                <td>Popis</td>
                <td><input type="text" name="name" value="<?= $this->name ?>"/></td>
            </tr>
            <tr>
                <td>Složka</td>
                <td><?php
                    $s = $this->select()
                              ->set($this->parent)
                              ->name('parent')
                              ->option('0', '--');
                    foreach ($this->dirs as $item) {
                        $s->option($item['id'], $item['text']);
                    }
                    echo $s;
                    ?></td>
            </tr>
            <tr>
                <td></td>
                <td>
                    <?= $this->submit('Upravit')->data('action', 'add') ?>
                    <a href="<?= $this->referer ?>">Zpět</a>
                </td>
            </tr>
        </table>
    </form>
</div>
