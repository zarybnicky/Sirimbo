<?= $this->header('Správa fotogalerie', 'Upload') ?>
<?= $this->notice($this->redirect()->getMessage()); ?>
<div class="container full">

    <form enctype="multipart/form-data" action="" method="post">
        <table>
            <tr>
                <td>Soubor</td>
                <td><?= $this->upload('files') ?></td>
            </tr>
            <tr>
                <td>Složka: </td>
                <td><?php
                    $s = $this->select('dir');
                    foreach ($this->dirs as $item) {
                        $s->option($item['id'], $item['text']);
                    }
                    echo $s;
                    ?></td>
            </tr>
            <tr>
                <td>
                    <?= $this->submit('Nahrát na server')->data('action', 'upload') ?>
                    <a href="/admin/galerie">Zpět</a>
                </td>
            </tr>
        </table>
    </form>
</div>
