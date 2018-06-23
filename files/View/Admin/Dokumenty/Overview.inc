<?= $this->header('Správa dokumentů') ?>

<div class="container full">
    <form action="/admin/dokumenty" method="POST">
        <?php
        if ($this->showMenu) {
            echo $this->menu()
                      ->content('Přejmenovat', 'edit', true)
                      ->content('Odstranit', 'remove', true);
        }
        echo '<div style="margin-right:150px">';
        echo $this->table()
                  ->data($this->data)
                  ->column('checkBox', '')
                  ->column('link', 'Jméno souboru')
                  ->column('name', 'Soubor')
                  ->column('category', 'Kategorie')
                  ->column('by', 'Nahrál');
        echo '</div>';
        ?>
    </form>
    <hr />
    <h2>Upload</h2>
    <form action="" method="post" enctype="multipart/form-data">
        <table>
            <tr>
                <td>Jméno:</td>
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
                <td>
                    <?= $this->submit('Upload')->data('action', 'upload') ?>
                </td>
            </tr>
        </table>
    </form>
</div>
