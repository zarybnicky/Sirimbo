<?= $this->header(
    'Správa oprávnění',
    ($this->action == 'add')
        ? 'Přidat uživatelskou skupinu'
        : 'Upravit uživatelskou skupinu'
) ?>
<div class="container full">
<?= $this->notice($this->redirect()->getMessage()) ?>
<form action="" method="POST">
<table>
    <tr>
        <td>Jméno: </td>
        <td colspan="<?= count(Settings::$permissionLevels) ?>">
            <?= $this->text('name', $this->name) ?>
        </td>
    </tr>
    <tr>
        <td>Popis: </td>
        <td colspan="<?= count(Settings::$permissionLevels) ?>">
            <?= $this->text('description', $this->description)->size(35) ?>
        </td>
    </tr>
    <?php
    foreach (Settings::$permissions as $name => $item) {
        echo '<tr>';
        echo '<td>', $item['name'], '</td>';
        foreach (Settings::$permissionLevels as $key => $level_name) {
            echo '<td>';
            if (isset($item[$key])) {
                echo $this->radio($name, $key)
                          ->set($item['default'] == $key)
                          ->label($level_name);
            }
            echo '</td>';
        }
        echo '</tr>';
    }
    ?>
</table>
<?= $this->submit($this->action == 'add' ? 'Přidat' : 'Upravit') ?>
<a href="/admin/permissions">Zpět</a>
</form>
</div>
