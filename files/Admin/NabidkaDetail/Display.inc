<h1>Správa nabídky</h1>
<?php
notice($this->redirect()->getMessage());
?>
<form action="" method="POST" class="centerWTable">
<button type="submit" name="action" value="add" style="display:none;">Enter</button>
<div class="n_wrapper">
<?php DisplayNabidka::viewNabidkaHeader($this->data, $this->obsazeno); ?>
<table>
<tr><td>Tanečník</td><td>Počet hodin</td></tr>
<?php
$userselect = $this->userSelect()
    ->users($this->users)
    ->type('par')
    ->idVar('p_id');
if (!empty($this->items)) {
    foreach ($this->items as $item) {
            echo '<tr>';
            post($item['ni_id'] . '-partner', $item['ni_partner']);
            echo '<td>';
            echo $userselect->name($item['ni_id'] . '-partner');
            echo '</td>';
            echo '<td><input type="text" name="', $item['ni_id'], '-hodiny" value="', $item['ni_pocet_hod'],
                '" size=1/></td>';
            echo '<td><button type="submit" name="remove" value="', $item['ni_id'], '">',
                'Odstranit</button></td>';
            echo '</tr>';
    }
}
?>
<tr>
<td><?php echo $userselect->name('add_partner');?></td>
<td><input type="text" name="add_hodiny" value=""  size=1/></td>
<td><button type="submit" name="action" value="add">Přidat</button></td>
</tr>
<tr><td><button type="submit" name="action" value="submit">Uložit</button></td></tr>
</table>
</div>
<a href="<?php echo Request::getReferer();?>">Zpět</a>
</form>