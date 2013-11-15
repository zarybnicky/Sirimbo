<h1>Správa dokumentů</h1>
<?php notice($this->redirect()->getMessage()); ?>

<h2>Přehled</h2>
<form action="" method="POST">
    <button type="submit" name="action" value="edit">Přejmenovat</button>
    <button type="submit" name="action" value="remove">Odstranit</button>
    <table>
        <tr>
            <td></td>
            <td>Jméno souboru</td>
            <td>Soubor</td>
            <td>Kategorie</td>
            <td>Uploadoval</td>
        </tr>
        <?php foreach ($this->dokumenty as $item) : ?>
        <tr>
            <td>
                <?= getCheckbox('dokumenty[]', $item['id'], false, false, true) ?>
            </td>
            <td>
                <a href="/member/download?id=<?= $item['id'] ?>"><?= $item['name'] ?></a>
            </td>
            <td>
                <?= $item['filename'] ?>
            </td>
            <td>
                <?= $item['category'] ?>
            </td>
            <td>
                <?= $item['fullName'] ?>
            </td>
        </tr>
        <?php endforeach; ?>
    </table>
</form>

<hr />

<h2>Upload</h2>
<form action="" method="post" enctype="multipart/form-data">
    <table>
        <tr>
            <td>Popis:</td>
            <td><input type="text" name="name" /></td>
        </tr>
        <tr>
            <td>Soubor:</td>
            <td><input type="file" name="file" /></td>
        </tr>
        <tr>
            <td>Kategorie</td>
            <td>
                <select name="kategorie">
                    <option value="1">Schůze, rady</option>
                    <option value="2">Soutěže</option>
                    <option value="3">Tábory</option>
                    <option value="0">Ostatní</option>
                </select>
            </td>
        </tr>
        <tr>
            <td></td>
            <td><button type="submit" name="action" value="upload">Upload</button></td>
        </tr>
    </table>
</form>