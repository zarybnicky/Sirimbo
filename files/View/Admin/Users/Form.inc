<?= $this->header(
    'Správa uživatelů',
    $this->action == 'add' ? 'Přidat uživatele' : 'Upravit uživatele'
) ?>
<div class="container full">
<form action="" method="POST">
<table>
    <tr>
        <td>Uživatel (*): </td>
        <td><input type="text" name="login" value="<?= $this->login ?>" <?php
            if ($this->action == 'edit') echo 'readonly="readonly"';
        ?>/></td>
        <td>Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků</td>
    </tr><tr>
        <td>Heslo (*): </td>
        <td><input type="text" name="pass" value="<?= $this->pass ?>" <?php
            if ($this->action == 'edit') echo 'disabled="disabled"';
        ?>/></td>
        <td>Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků</td>
    </tr><tr>
        <td>Jméno (*): </td>
        <td><input type="text" name="jmeno" value="<?= $this->jmeno ?>" /></td>
        <td>Max. 40 znaků</td>
    </tr><tr>
        <td>Příjmení (*): </td>
        <td><input type="text" name="prijmeni" value="<?= $this->prijmeni ?>" /></td>
        <td>Max. 40 znaků</td>
    </tr><tr>
        <td>Pohlaví (*): </td>
        <td>
            <?= $this->radio('pohlavi', 'm')->set($this->pohlavi == 'm')->label('Muž') ?>&nbsp;
            <?= $this->radio('pohlavi', 'f')->set($this->pohlavi == 'f')->label('Žena') ?>
        </td>
        <td></td>
    </tr><tr>
        <td>E-mail (*): </td>
        <td><input type="text" name="email" value="<?= $this->email ?>" /></td>
        <td>Max. 50 znaků, ve formátu e-mail adresy</td>
    </tr><tr>
        <td>Telefon (*): </td>
        <td><input type="text" name="telefon" value="<?= $this->telefon ?>" /></td>
        <td>Pouze číslice a +, max. 13 znaků</td>
    </tr><tr>
        <td>Datum narození: (*)</td>
        <td><?= $this->date('narozeni', $this->narozeni)->textBox() ?></td>
        <td></td>
    </tr><tr>
        <td>Poznámky: </td>
        <td><textarea rows="15" cols="50" name="poznamky"><?= $this->poznamky ?></textarea></td>
        <td></td>
    </tr><tr>
        <td>Uzamknout: </td>
        <td><?= $this->checkbox('lock', 'lock')->set($this->lock) ?></td>
        <td></td>
    </tr><tr>
        <td>Ban: </td>
        <td><?= $this->checkbox('ban', 'ban')->set($this->ban) ?></td>
        <td></td>
    </tr><tr>
        <td>Aktivní tanečník: </td>
        <td><?= $this->checkbox('dancer', 'dancer')->set($this->dancer) ?></td>
        <td></td>
    </tr><tr>
        <td>Systémový uživatel: </td>
        <td><?= $this->checkbox('system', 'system')->set($this->system) ?></td>
        <td></td>
    </tr><tr>
        <td>Oprávnění (*): </td>
        <td><?php
        $s = $this->select('group')->set($this->group);
        foreach ($this->groups as $item)
            $s->option($item['id'], $item['name']);
        echo $s;
        ?></td>
        <td></td>
    </tr><tr>
        <td>Skupina (*): </td>
        <td><?php
        foreach ($this->skupiny as $item) {
            echo $this->radio('skupina', $item['id'])
                      ->set($this->skupina == $item['id'])
                      ->label($this->colorbox($item['color'], $item['popis']) . $item['popis']);
            echo '<br/>';
        }?></td>
        <td></td>
    </tr><tr>
        <td><?= $this->submit($this->action == 'add' ? 'Přidat' : 'Upravit') ?></td>
        <td>Pole označená (*) jsou povinná</td>
        <td></td>
    </tr>
</table>
</form>
<a class="btn btn-outline-primary" href="/admin/users">Zpět</a>
</div>
