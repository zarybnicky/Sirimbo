<h1>Správa oprávnění</h1>
<?php
echo '<h2>', ($this->action == 'add' ? 'Přidat uživatelskou skupinu' : 'Upravit uživatelskou skupinu'), '</h2>';
notice($this->redirect()->getMessage());
?>
<form action="" method="POST">
<table>
    <tr>
        <td>Jméno: </td>
        <td colspan="<?php echo count($this->permissions);?>">
            <input type="text" name="name" value="<?php echo post('name'); ?>" />
        </td>
    </tr><tr>
        <td>Popis: </td>
        <td colspan="<?php echo count($this->permissions);?>">
            <textarea rows="7" cols="60" name="description"><?php echo post("description"); ?></textarea>
        </td>
    </tr><?php
    foreach ($this->permissions as $name => $item) {
        echo '<tr>';
        echo '<td>', $item['name'], '</td>';
        foreach ($this->permissionLevels as $key => $level_name) {
            echo '<td>';
            if (isset($item[$key])) {
                echo '<label>', getRadio($name, $key, $item['default'] == $key), $level_name, '</label>';
            }
            echo '</td>';
        }
        echo '</tr>';
    }
    ?>
</table>
<button type="submit" name="action"><?php echo ($this->action == 'add' ? 'Přidat' : 'Upravit'); ?></button>
<a href="/admin/permissions">Zpět</a>
</form>