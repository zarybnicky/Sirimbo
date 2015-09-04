<?= $this->header(
    'Správa aktualit',
    ($this->action == 'add' ? 'Přidat' : 'Upravit') . ' článek'
) ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()) ?>
    <form action="" method="POST">
        <table style="width:100%">
            <tr>
                <td>Jméno (*): </td>
                <td>
                    <input type="text"
                           name="name"
                           value="<?= $this->name ?>"
                           style="width:98%"/>
                </td>
                <td class="r">Max. 255 znaků</td>
            </tr><tr>
                <td>Kategorie (*): </td>
                <td>
                    <?= $this->radio('category', 1)
                             ->set($this->category == 1)
                             ->label('Články') ?>
                    &nbsp;
                    <?= $this->radio('category', 3)
                             ->set($this->category == 3)
                             ->label('Krátké zprávy') ?>
                </td>
                <td></td>
            </tr><tr>
                <td>Shrnutí: </td>
                <td></td>
                <td class="r">20 - 50 slov</td>
            </tr><tr>
                <td colspan="3">
                    <textarea rows="5"
                              cols="60"
                              name="summary"
                              style="width:99%"><?= $this->summary ?></textarea>
                </td>
            </tr><tr>
                <td>Text: </td>
                <td></td>
                <td></td>
            </tr><tr>
                <td colspan="3">
                    <textarea name="text"
                              id="text"
                              rows="30"
                              cols="60"
                              style="width:99%"><?= $this->text ?></textarea>
                    <script src="/scripts/ckeditor/ckeditor.js"></script>
                    <script>
                     CKEDITOR.config.allowedContent = true;
                     CKEDITOR.replace('text');
                    </script>
                </td>
            </tr><tr>
                <td colspan="2">
                    <a href="/admin/aktuality">Zpět</a>
                    <?= $this->submit('Přidat fotky')->data('action', 'foto') ?>
                    <?php if ($this->action == 'add'): ?>
                        <?= $this->submit('Přeskočit a uložit')->data('action', 'save') ?>
                    <?php else: ?>
                        <?= $this->submit('Upravit')->data('action', 'save') ?>
                    <?php endif; ?>
                </td>
                <td class="r">Pole označená (*) jsou povinná</td>
            </tr>
        </table>
    </form>
</div>
