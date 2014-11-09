<div class="container full">
<h1>Správa fotogalerie</h1>
<?php echo $this->notice($this->redirect()->getMessage()); ?>
<h2>Upload</h2>
<form enctype="multipart/form-data" action="" method="post">
<table>
    <tr>
        <td>Soubor</td>
        <td><?php
        echo $this->upload('files');
        ?></td>
    </tr>
    <tr>
        <td>Složka: </td>
        <td><?php
    $s = $this->select()->post()->name('dir');
    foreach ($this->dirs as $item) {
        $s->option($item['id'], $item['text']);
    }
    echo $s;
        ?></td>
    </tr>
    <tr>
        <td>
            <button type="submit" name="action" value="upload">Nahrát na servers</button>
            <a href="/admin/galerie">Zpět</a>
        </td>
    </tr>
</table>
</form>
</div>
