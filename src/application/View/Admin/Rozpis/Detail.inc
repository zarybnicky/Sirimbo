<h1>Správa rozpisů</h1>
<?php notice($this->redirect()->getMessage()); ?>
<form action="" method="POST">
<button type="submit" name="action" value="enter" style="display:none;">Enter</button>
<div class="r_wrapper">
    <?php $this->render('src/application/View/Admin/Rozpis/Header.inc', $this->data); ?>
    <table>
        <tr>
            <td>Tanečník</td>
            <td>Od</td>
            <td>Do</td>
            <td></td>
            <td>Zámek</td>
        </tr>
        <?php
        $userselect = $this->userSelect()
            ->idVar('p_id')
            ->type('par')
            ->users($this->users);
        foreach ($this->items as $item) :
            post($item['ri_id'] . '-partner', $item['ri_partner']);
        ?>
        <tr>
            <td>
                <?= $userselect->name($item['ri_id'] . '-partner'); ?>
            </td>
            <td>
                <input type="text" name="<?= $item['ri_id'] ?>-od" size="5" value="<?= $item['o'] ?>" />
            </td>
            <td>
                <input type="text" name="<?= $item['ri_id'] ?>-do" size="5" value="',
                    formatTime($item['ri_do'], 1), '"/>
            </td>
            <td>
                <button type="submit" name="remove" value="<?= $item['ri_id'] ?>">Odstranit</button>
            </td>
            <td>
                <?= getCheckbox($item['ri_id'] . '-lock', 'lock', $item['ri_lock']) ?>
            </td>
        </tr>
        <?php endforeach; ?>
        <tr>
            <td>
                <?= $userselect->name('add_partner'); ?>
            </td>
            <td>
                <input type="text" name="add_od" size="5" value="" />
            </td>
            <td>
                <input type="text" name="add_do" size="5" value="" />
            </td>
            <td>
                <button type="submit" name="action" value="add">Přidat</button>
            </td>
            <td>
                <input type="checkbox" name="add_lock" value="lock" />
            </td>
        </tr>
        <tr>
            <td>
                <button type="submit" name="action" value="submit">Uložit</button>
            </td>
        </tr>
    </table>
</div>
<br/>
<h2>Nabídky</h2>
<?php
$s = $this->select()
    ->post()->name('n_id')
    ->option('none', '---');
foreach ($this->nabidky as $item) {
    $s->option($item['id'], $item['date'] . ': ' . $item['fullName']);
}
echo $s;
?>
<button type="submit" name="action" value="select">Vybrat nabídku</button>
<?php if ($this->currentNabidka) : ?>
    <div class="n_wrapper">
    <?php $this->render('src/application/View/Admin/Nabidka/Header.inc', $this->currentNabidka); ?>

    <?php if (!empty($items)) : ?>
        <table>
            <tr>
                <td>Tanečník</td>
                <td>Počet hodin</td>
            </tr>
        <?php foreach ($items as $item) : ?>
            <tr>
                <td><?= $item['fullName'] ?></td>
                <td><?= $item['pocetHod'] ?></td>
            </tr>
        <?php endforeach; ?>
        </table>
    <?php endif; ?>
    </div>
<?php endif; ?>
<br/>
<br/>
<h2>Pomocník</h2>
<button type="submit" name="generate" value="overlap">Opravit přesahy</button><br/>
<button type="submit" name="generate" value="gen_add">Přidat</button>
<input type="text" name="gen_add_hod" size="2" /> hodin po
<input type="text" name="gen_add_len" size="3" /> minutách od
<input type="text" name="gen_add_od" size="5" /> hodin
</form>
<br/>
<a href="/admin/rozpis">Zpět</a>
