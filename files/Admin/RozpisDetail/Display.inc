<h1>Správa rozpisů</h1>
<?php
notice($this->redirect()->getMessage());
?>
<form action="" method="POST">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<div class="r_wrapper">
<?php DisplayRozpis::viewRozpisHeader($this->data); ?>
<table>
<tr><td>Tanečník</td><td>Od</td><td>Do</td><td></td><td>Zámek</td></tr>
<?php
$userselect = $this->userSelect()
    ->idVar('p_id')
    ->type('par')
    ->users($this->users);
if (!empty($this->items)) {
    foreach ($this->items as $item) {
        echo '<tr>';
        echo '<td>';
        post($item['ri_id'] . '-partner', $item['ri_partner']);
        echo $userselect->name($item['ri_id'] . '-partner');
        echo '</td>';
        echo '<td><input type="text" name="', $item['ri_id'], '-od" size="5" value="',
            formatTime($item['ri_od'], 1), '"/></td>';
        echo '<td><input type="text" name="', $item['ri_id'], '-do" size="5" value="',
            formatTime($item['ri_do'], 1), '"/></td>';
        echo '<td><button type="submit" name="remove" value="', $item['ri_id'],
            '">Odstranit</button></td>';
        echo '<td>', getCheckbox($item['ri_id'] . '-lock', 'lock', $item['ri_lock']), '</td>';
        echo '</tr>';
    }
}
?>
<tr>
<td><?php
echo $userselect->name('add_partner');
?></td>
<td><input type="text" name="add_od" size="5" value="" /></td>
<td><input type="text" name="add_do" size="5" value="" /></td>
<td><button type="submit" name="action" value="add">Přidat</button></td>
<td><input type="checkbox" name="add_lock" value="lock" /></td>
</tr>
<tr><td><button type="submit" name="action" value="submit">Uložit</button></td></tr>
</table>
</div>
<br/>
<h2>Nabídky</h2>
<?php
$nabidky = DBNabidka::getNabidka();

$s = $this->select()
    ->post()->name('n_id')
    ->option('none', '--');
foreach ($nabidky as $item)
    $s->option($item['n_id'],
        formatDate($item['n_od']) . ($item['n_od'] != $item['n_do'] ?
            (' - ' . formatDate($item['n_do'])) : '') .
        ': ' . $item['u_jmeno'] . ' ' . $item['u_prijmeni']);
echo $s;
?>
<button type="submit" name="action" value="select">Vybrat nabídku</button>
<?php
if (is_numeric(post('n_id'))) {
    $n_id = post('n_id');
    if (!($nabidka = DBNabidka::getSingleNabidka($n_id))) {
        echo 'Nabídka se zadaným ID neexistuje!';
    } else {
        echo '<div class="n_wrapper">';
        DisplayNabidka::viewNabidkaHeader($nabidka,
            DBNabidka::getNabidkaItemLessons($n_id));
        
        $items = DBNabidka::getNabidkaItem($n_id);
        if (!empty($items)) {
            echo '<table>';
            echo '<tr><td>Tanečník</td><td>Počet hodin</td></tr>';
            foreach ($items as $item) {
                echo '<tr>';
                echo '<td>', echoFullJmeno($item), '</td>';
                echo '<td>', $item['ni_pocet_hod'], '</td>';
                echo '</tr>';
            }
            echo '</table>';
        }
        echo '</div>';
    }
}
?>
<br/><br/>
<h2>Pomocník</h2>
<button type="submit" name="generate" value="overlap">Opravit přesahy</button><br/>
<button type="submit" name="generate" value="gen_add">Přidat</button>
<input type="text" name="gen_add_hod" size="2" /> hodin po
<input type="text" name="gen_add_len" size="3" /> minutách od
<input type="text" name="gen_add_od" size="5" /> hodin
</form>
<br/>
<a href="/admin/rozpis">Zpět</a>
