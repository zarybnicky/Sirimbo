<?= $this->header('Správa videí')
         ->subheader($this->header) ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()) ?>
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
                    <button type="submit">Upravit</button>
                    <a href="/admin/video">Zpět</a>
                </td>
                <td>Pole označená (*) jsou povinná</td>
            </tr>
        </table>
    </form>
</div>