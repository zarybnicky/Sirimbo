<h1>Správa akcí</h1>
<?php notice($this->redirect()->getMessage()); ?>

<form action="" method="POST" class="centerWTable">
    <?php echo $this->render('src/application/View/Member/Akce/Header.inc', $this->data);?>

    <table>
        <tr>
            <td>Jméno<br/>souboru</td>
            <td>Kategorie</td>
        </tr>
        <?php foreach ($this->dokumentyCurrent as $item) : ?>
        <tr>
            <td>
                <?= $item['name'] ?>
            </td>
            <td>
                <?= $item['type'] ?>
            </td>
            <td>
                <button type="submit" name="remove" value="<?= $item['id'] ?>">
                    Odstranit
                </button>
            </td>
        </tr>
        <?php endforeach; ?>
        <tr>
            <td>
                <?php
                $select = new \TKOlomouc\View\Helper\Select('add-id');
                $select->option('none', '---');
                foreach ($this->dokumentyAll as $item) {
                    $select->option($item['id'], "{$item['name']} ({$item['type']})");
                }
                echo $select;
                ?>
            </td>
            <td>
                <button type="submit" name="add" value="add">Přidat</button>
            </td>
        </tr>
    </table>
    <a href="/admin/akce">Zpět</a>
</form>