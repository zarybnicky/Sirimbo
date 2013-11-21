<h1>Správa fotogalerie</h1>
<?php
notice($this->redirect()->getMessage());
?>
<h2>Upload</h2>
<form enctype="multipart/form-data" action="" method="post">
<table>
    <tr>
        <td>Soubor</td>
        <td><?php
        echo (new TKOlomouc\View\Helper\Upload('files'));
        ?></td>
    </tr>
    <tr>
        <td>Složka: </td>
        <td><?php
    $s = (new TKOlomouc\View\Helper\Select('dir'))->post();
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