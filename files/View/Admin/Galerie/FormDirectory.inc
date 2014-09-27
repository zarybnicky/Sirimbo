<div class="container">
<h1>Správa fotogalerie</h1>
<?php echo $this->notice($this->redirect()->getMessage()); ?>
<form action="" method="post">
<h2>Přidat složku</h2>
<table>
    <tr>
        <td>Název</td>
        <td><input type="text" name="name" value="<?= post('name');?>"/></td>
    </tr>
    <tr>
        <td>Nadsložka</td>
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
        <td>Skrytá složka</td>
        <td><?= $this->checkbox('hidden', '1');?></td>
    </tr>
    <tr>
        <td></td>
        <td>
            <button type="submit" name="action" value="add"><?=
                $this->action == 'add' ? 'Přidat složku' : 'Upravit složku'
            ?></button>
            <a href="/admin/galerie">Zpět</a>
        </td>
    </tr>
</table>
</form>
</div>
