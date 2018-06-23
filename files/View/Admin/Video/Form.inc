<?= $this->header('Správa videí', $this->header) ?>

<div class="container full">
    <form action="" method="post">
        <table style="width:100%">
            <tr>
                <td style="width:120px">Jméno (*):</td>
                <td>
                    <input type="text"
                           name="name"
                           value="<?=$this->name ?>"
                           style="width:98%" />
                </td>
            </tr>
            <tr>
                <td>Popis:</td>
                <td>
                    <textarea name="text"
                              rows="5"
                              style="width:98%"><?= $this->text ?></textarea>
                </td>
            </tr>
            <tr>
                <td>URI:</td>
                <td>
                    <input type="text"
                           name="uri"
                           value="<?= $this->uri ?>"
                           style="width:98%" />
                </td>
            </tr>
            <tr>
                <td>
                    <?= $this->submit('Upravit') ?>
                    <a href="/admin/video">Zpět</a>
                </td>
                <td>Pole označená (*) jsou povinná</td>
            </tr>
        </table>
    </form>
</div>
