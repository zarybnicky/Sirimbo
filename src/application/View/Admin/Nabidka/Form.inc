<h1>Správa nabídky</h1>
<?php
echo '<h2>', ($this->action == 'add' ? 'Přidat nabídku' : 'Upravit nabídku'), '</h2>';
notice($this->redirect()->getMessage());
?>
<form action="" method="POST">
<table>
    <tr>
        <td>Trenér (*): </td>
        <td><?php if (!Permissions::check('nabidka', P_ADMIN)) { ?>
        <select name="trener">
            <option value="<?php echo User::getUserID();?>">Já</option>
        </select><?php
        } else {
            echo $this->userSelect()
                ->name('trener')
                ->users($this->users);
        } ?></td>
    </tr><tr>
        <td>Počet hodin (*): </td>
        <td><input type="text" name="pocet_hod" value="<?php echo post("pocet_hod"); ?>" /></td>
    </tr><tr>
        <td>Max. počet hodin/pár (*): </td>
        <td><input type="text" name="max_pocet_hod" value="<?php echo post("max_pocet_hod"); ?>" /></td>
    </tr><tr>
        <td></td>
        <td>(0 = neomezeně)</td>
    </tr><tr>
        <td>Od: (*)</td>
        <td><?php echo $this->date('od')->textBox();?></td>
    </tr><tr>
        <td>Do: </td>
        <td><?php echo $this->date('do')->textBox();?></td>
    </tr><tr>
        <td></td>
        <td>(pokud je "Do" prázdné, počítá se jako "Od")</td>
    </tr><tr>
        <td>Viditelný: </td>
        <td><label><?php echo (Permissions::check('nabidka', P_ADMIN) ?
            getCheckbox('visible', 'visible')
            : ($this->visible ? '&#10003;' : '&#10799;'));?></label></td>
    </tr><tr>
        <td>Uzamknout: </td>
        <td><label><?php echo getCheckbox('lock');?></label></td>
        <td></td>
    </tr><tr>
        <td><button type="submit"><?php
            echo ($this->action == 'add' ? 'Přidat' : 'Upravit');
            echo getReturnInput();
        ?></button></td>
        <td>Pole označená (*) jsou povinná</td>
    </tr>
</table>
</form>

<a href="<?php echo $this->returnURL;?>">Zpět</a>