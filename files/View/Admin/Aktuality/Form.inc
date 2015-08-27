<?= $this->header('Správa aktualit')
         ->subheader(($this->action == 'add' ? 'Přidat' : 'Upravit') . ' článek') ?>
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
                    <label><?= $this->radio('category', 1)
                                    ->set($this->category == 1);?>
                        Články
                    </label>&nbsp;
                    <label><?= $this->radio('category', 3)
                                    ->set($this->category == 3);?>
                        Krátké zprávy
                    </label>
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
                    <button type="submit" name="action" value="foto">Přidat fotky</button>
                    <?php if ($this->action == 'add'): ?>
                        <button type="submit" name="action" value="save">Přeskočit a uložit</button>
                    <?php else: ?>
                        <button type="submit">Upravit</button>
                    <?php endif; ?>
                </td>
                <td class="r">Pole označená (*) jsou povinná</td>
            </tr>
        </table>
    </form>
</div>
