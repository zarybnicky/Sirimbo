<div class="container">
<h1>Správa párů</h1>
<h2>Změna třídy a bodů</h2>
<?php echo $this->notice($this->redirect()->getMessage());?>
<form action="" method="POST">
Pár: <?php echo $this->fullName;?>
<table>
<tr><td>Standart:</td><td></td></tr>
<tr>
    <td>Třída:</td>
    <td><?php
        echo $this->select()
            ->post()->name('stt-trida')
            ->options(array('Z', 'H', 'D', 'C', 'B', 'A', 'M'), true, true);
    ?></td>
</tr>
<tr>
    <td>Body:</td>
    <td><input type="text" name="stt-body" value="<?php echo post('stt-body');?>" /></td>
</tr>
<tr><td>Finále:</td>
    <td><input type="text" name="stt-finale" value="<?php echo post('stt-finale');?>" /></td>
</tr>
<tr><td>Latina:</td><td></td></tr>
<tr>
    <td>Třída:</td>
    <td><?php
        echo $this->select()
            ->post()->name('lat-trida')
            ->options(array('Z', 'H', 'D', 'C', 'B', 'A', 'M'), true, true);
    ?></td>
</tr>
<tr>
    <td>Body:</td>
    <td><input type="text" name="lat-body" value="<?php echo post('lat-body');?>" /></td>
</tr>
<tr>
    <td>Finále:</td>
    <td><input type="text" name="lat-finale" value="<?php echo post('lat-finale');?>" /></td>
</tr>
</table>
    <button type="submit" name="action" value="edit_confirm">Změnit třídu a body</button>
</form>

<a href="/admin/pary">Zpět</a>
</div>
