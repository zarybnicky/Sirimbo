<?= $this->header('Správa párů') ?>
<?= $this->subheader('Změna třídy a bodů') ?>
<div class="container full">
<?= $this->notice($this->redirect()->getMessage());?>
<form action="" method="POST">
Pár: <?= $this->fullName;?>
<table>
<tr><td>Standart:</td><td></td></tr>
<tr>
    <td>Třída:</td>
    <td><?= $this->select()
                 ->post()
                 ->name('stt-trida')
                 ->set($this->stt_trida)
                 ->options(array('Z', 'H', 'D', 'C', 'B', 'A', 'M'), true, true);
    ?></td>
</tr>
<tr>
    <td>Body:</td>
    <td><input type="text" name="stt-body" value="<?= $this->stt_body ?>" /></td>
</tr>
<tr><td>Finále:</td>
    <td><input type="text" name="stt-finale" value="<?= $this->stt_finale ?>" /></td>
</tr>
<tr><td>Latina:</td><td></td></tr>
<tr>
    <td>Třída:</td>
    <td><?= $this->select()
                 ->post()
                 ->name('lat-trida')
                 ->set($this->lat_trida)
                 ->options(array('Z', 'H', 'D', 'C', 'B', 'A', 'M'), true, true);
    ?></td>
</tr>
<tr>
    <td>Body:</td>
    <td><input type="text" name="lat-body" value="<?= $this->lat_body ?>" /></td>
</tr>
<tr>
    <td>Finále:</td>
    <td><input type="text" name="lat-finale" value="<?= $this->lat_finale ?>" /></td>
</tr>
</table>
    <button type="submit" name="action" value="edit_confirm">Změnit třídu a body</button>
</form>

<a href="/admin/pary">Zpět</a>
</div>
