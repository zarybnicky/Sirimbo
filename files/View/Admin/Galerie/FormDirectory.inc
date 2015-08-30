<?= $this->header(
    'Správa fotogalerie',
    $this->action == 'add' ? 'Přidat složku' : 'Upravit složku'
) ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()); ?>
    <form action="" method="post">
        <table>
            <tr>
                <td>Název</td>
                <td><input type="text" name="name" value="<?= $this->name ?>"/></td>
            </tr>
            <tr>
                <td>Nadsložka</td>
                <td><?php
                    $s = $this->select('parent')
                              ->set($this->parent)
                              ->option('0', '--');
                    foreach ($this->dirs as $item) {
                        $s->option($item['id'], $item['text']);
                    }
                    echo $s;
                    ?></td>
            </tr>
            <tr>
                <td>Skrytá složka</td>
                <td><?= $this->checkbox('hidden', '1')->set($this->hidden) ?></td>
            </tr>
            <tr>
                <td></td>
                <td>
                    <?= $this->submit($this->action == 'add' ? 'Přidat složku' : 'Upravit složku') ?>
                    <a href="/admin/galerie">Zpět</a>
                </td>
            </tr>
        </table>
    </form>
</div>
