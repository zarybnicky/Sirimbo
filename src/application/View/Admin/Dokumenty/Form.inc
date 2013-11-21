<h1>Správa dokumentů</h1>
<?php notice($this->redirect()->getMessage()); ?>

<h2>Upravit soubor</h2>
<form action="" method="POST">
    <table>
        <tr>
            <td>
                Popis: (*)
            </td>
            <td>
                <input type="text" name="name" value="<?= post('name') ?>" />
            </td>
        </tr>
        <tr>
            <td>
                Kategorie: (*)
            </td>
            <td>
                <?php
                echo (new TKOlomouc\View\Helper\Select('category'))
                    ->option('1', 'Schůze, rady')
                    ->option('2', 'Soutěže')
                    ->option('3', 'Tábory')
                    ->option('0', 'Ostatní');
                ?>
            </td>
        </tr>
        <tr>
            <td>
                <button type="submit">Upravit</button>
                <a href="/admin/dokumenty">Zpět</a>
            </td>
            <td></td>
        </tr>
    </table>
</form>