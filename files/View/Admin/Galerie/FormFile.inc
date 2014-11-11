<?= $this->header('Správa fotogalerie') ?>
<div class="container full">
<?php echo $this->notice($this->redirect()->getMessage()); ?>
<form action="" method="post">
<h2>Upravit soubor</h2>
<table>
    <tr>
        <td>Popis</td>
        <td><input type="text" name="name" value="<?= post('name');?>"/></td>
    </tr>
    <tr>
        <td>Složka</td>
        <td><?php
$s = $this->select()
    ->post()
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
            <button type="submit" name="action" value="add">Upravit</button>
            <a href="<?= $this->referer ?>">Zpět</a>
        </td>
    </tr>
</table>
</form>
</div>
