<h1>Profil</h1>
<h2>Změna třídy a bodů</h2>
<?php
echo $this->partnerRequest()->getAll();
echo $this->notice($this->redirect()->getMessage());
?>
<form action="" method="post">
<table>
<tr><td>Standart:</td><td></td><td></td></tr>
<tr>
    <td>Třída:</td>
    <td><?php
echo $this->select()
    ->post()->name('stt-trida')
    ->options(array('Z', 'H', 'D', 'C', 'B', 'A', 'M'), true, true);
    ?></td>
    <td></td>
</tr>
<tr>
    <td>Body:</td>
    <td><input type="text" name="stt-body" value="<?php echo post('stt-body');?>" /></td>
    <td>0 až 1000</td>
</tr>
<tr>
    <td>Finále:</td>
    <td><input type="text" name="stt-finale" value="<?php echo post('stt-finale');?>" /></td>
    <td>0 až 10</td>
</tr>
<tr><td>Latina:</td><td></td><td></td></tr>
<tr>
    <td>Třída:</td>
    <td><?php
        echo $this->select()
            ->post()->name('lat-trida')
            ->options(array('Z', 'H', 'D', 'C', 'B', 'A', 'M'), true, true);
    ?></td>
    <td></td>
</tr>
<tr>
    <td>Body:</td>
    <td><input type="text" name="lat-body" value="<?php echo post('lat-body');?>" /></td>
    <td>0 až 1000</td>
</tr>
<tr>
    <td>Finále:</td>
    <td><input type="text" name="lat-finale" value="<?php echo post('lat-finale');?>" /></td>
    <td>0 až 10</td>
</tr>
</table>
<button type="submit" name="action">Změnit třídu a body</button>
<a href="/member/profil/par">Zpět</a>
</form>
