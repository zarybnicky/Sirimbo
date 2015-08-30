<?= $this->header(
    'Správa nástěnky',
    $this->action == 'add' ? 'Přidat příspěvek' : 'Upravit příspěvek'
) ?>
<div class="container full">
<?= $this->notice($this->redirect()->getMessage()) ?>
<form action="" method="POST">
<table>
    <tr>
        <td>Nadpis:</td>
        <td><input type="text" name="nadpis" value="<?= $this->nadpis ?>" /></td>
    </tr><tr>
        <td>Text:</td>
        <td><textarea cols="50" rows="15" name="text"><?= $this->text ?></textarea></td>
    </tr><tr>
        <td>Uzamknout:</td>
        <td><?= $this->checkbox('lock')->set($this->lock) ?></td>
    </tr><tr>
        <td>Skupiny:</td>
        <td>
            <?php
            foreach ($this->skupiny as $item) {
                if (true) //$item['us_platba_mesic'] > 0 || $item['us_platba_ctvrtrok'] > 0 || $item['us_platba_pulrok'] > 0
                    echo $this->checkbox('sk-' . $item['s_id'], '1'), ' ',
                $this->colorbox($item['s_color_rgb'], $item['s_description']), ' ',
                $item['s_name'], '<br/>';
            }
            ?>
            <input type="hidden"
                   name="referer"
                   value="<?= $this->referer ?>" />
        </td>
    </tr><tr>
        <td><?= $this->submit($this->action == 'add' ? 'Přidat' : 'Upravit') ?></td>
        <td>Pole označená (*) jsou povinná</td>
    </tr>
</table>
</form>

<a href="<?= $this->returnURI?>">Zpět</a>
</div>
