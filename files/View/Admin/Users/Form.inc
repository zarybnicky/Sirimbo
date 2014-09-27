<h1>Správa uživatelů</h1>
<?php
echo '<h2>', ($this->action == 'add' ? 'Přidat uživatele' : 'Upravit uživatele'), '</h2>';
notice($this->redirect()->getMessage());
?>
<form action="" method="POST">
<table>
    <tr>
        <td>Uživatel (*): </td>
        <td><input type="text" name="login" value="<?php echo post('login'); ?>" <?php
            if ($this->action == 'edit') echo 'readonly="readonly"';
        ?>/></td>
        <td>Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků</td>
    </tr><tr>
        <td>Heslo (*): </td>
        <td><input type="text" name="pass" value="<?php echo post('pass'); ?>" <?php
            if ($this->action == 'edit') echo 'disabled="disabled"';
        ?>/></td>
        <td>Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků</td>
    </tr><tr>
        <td>Jméno (*): </td>
        <td><input type="text" name="jmeno" value="<?php echo post("jmeno"); ?>" /></td>
        <td>Max. 40 znaků</td>
    </tr><tr>
        <td>Příjmení (*): </td>
        <td><input type="text" name="prijmeni" value="<?php echo post("prijmeni"); ?>" /></td>
        <td>Max. 40 znaků</td>
    </tr><tr>
        <td>Pohlaví (*): </td>
        <td>
            <label><?php echo $this->radio('pohlavi', 'm');?>Muž</label>&nbsp;
            <label><?php echo $this->radio('pohlavi', 'f');?>Žena</label>
        </td>
        <td></td>
    </tr><tr>
        <td>E-mail (*): </td>
        <td><input type="text" name="email" value="<?php echo post("email"); ?>" /></td>
        <td>Max. 50 znaků, ve formátu e-mail adresy</td>
    </tr><tr>
        <td>Telefon (*): </td>
        <td><input type="text" name="telefon" value="<?php echo post("telefon"); ?>" /></td>
        <td>Pouze číslice a +, max. 13 znaků</td>
    </tr><tr>
        <td>Datum narození: (*)</td>
        <td><?php echo $this->date('narozeni')->textBox(); ?></td>
        <td></td>
    </tr><tr>
        <td>Poznámky: </td>
        <td><textarea rows="15" cols="50" name="poznamky"><?php echo post("poznamky"); ?></textarea></td>
        <td></td>
    </tr><tr>
        <td>Uzamknout: </td>
        <td><label><?php echo getCheckbox('lock', 'lock');?></label></td>
        <td></td>
    </tr><tr>
        <td>Ban: </td>
        <td><label><?php echo getCheckbox('ban', 'ban');?></label></td>
        <td></td>
    </tr><tr>
        <td>Aktivní tanečník: </td>
        <td><label><?php echo getCheckbox('dancer', 'dancer');?></label></td>
        <td></td>
    </tr><tr>
        <td>Systémový uživatel: </td>
        <td><label><?php echo getCheckbox('system', 'system');?></label></td>
        <td></td>
    </tr><tr>
        <td>Oprávnění (*): </td>
        <td><?php
        $s = $this->select()->post()->name('group');
        foreach ($this->groups as $item)
            $s->option($item['id'], $item['name']);
        echo $s;
        ?></td>
        <td></td>
    </tr><tr>
        <td>Skupina (*): </td>
        <td><?php
        foreach ($this->skupiny as $item) {
            echo getColorBox($item['color'], $item['popis']);
            echo '<label>', $this->radio('skupina', $item['id']), $item['popis'], '</label><br/>';
        }?></td>
        <td></td>
    </tr><tr>
        <td><button type="submit"><?php
            echo $this->action == 'add' ? 'Přidat' : 'Upravit';
        ?></button></td>
        <td>Pole označená (*) jsou povinná</td>
        <td></td>
    </tr>
</table>
</form>
<a href="/admin/users">Zpět</a>
