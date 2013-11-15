<h1>Správa aktualit</h1>
<?php
notice($this->redirect()->getMessage());
?>
<form action="" method="POST">
    <?php
    if (!TISK) {
        echo $this->menu()
            ->content('Přidat', 'aktuality/add')
            ->content('Upravit', 'edit', true)
            ->content('Upravit galerii', 'foto', true)
            ->content('Odebrat', 'remove', true);
    }
    ?>
    <div style="margin-right:150px">
        <table style="width:100%">
            <tr>
                <td></td>
                <td>Jméno</td>
                <td>Kategorie</td>
            </tr>
            <?php foreach ($this->data as $item) : ?>
            <tr>
                <td>
                    <?= getCheckbox('aktuality[]', $item['id'], false, false, $item['canEdit']) ?>
                <td>
                    <?= $item['name'] ?>
                </td>
                <td>
                    <?= $item['type']?>
                </td>
            </tr>
            <?php endforeach; ?>
        </table>
    </div>
</form>