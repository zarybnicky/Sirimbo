<h1>Správa anket</h1>
<?php notice($this->redirect()->getMessage()); ?>

<h2>Přehled</h2>
<form action="" method="POST">
    <?php
    if (!TISK) {
        echo $this->menu()
            ->content('Přidat', 'ankety/add')
            ->content('Upravit', 'edit', true)
            ->content('Odstranit', 'remove', true)
            ->content('Uložit', 'save', true);
    }
    ?>
    <table>
        <tr>
            <td></td>
            <td>Jméno</td>
            <td>Viditelný</td>
        </tr>
        <?php foreach ($this->data as $item) : ?>
        <tr>
            <td>
                <?= getCheckbox('ankety[]', $item['id'], false, false, true) ?>
            </td>
            <td>
                <?= $item['ak_jmeno'] ?>
            </td>
            <td>
                <?= getCheckbox($item['id'], '1', $item['visible']) ?>
            </td>
        </tr>
        <?php endforeach; ?>
    </table>
</form>