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
    foreach ($this->settings as $item) {
        echo '<tr>';
        echo '<td>', $item['name'], '</td>';
        foreach ($item['items'] as $level) {
            echo "<td>$level</td>";
        }
        echo '</tr>';
    }
    ?>
</table>
<?= $this->submit($this->action == 'add' ? 'Přidat' : 'Upravit') ?>
<a href="/admin/permissions">Zpět</a>
</form>
</div>
