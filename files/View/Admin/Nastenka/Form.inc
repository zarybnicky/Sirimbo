<?= $this->header(
    'Správa nástěnky',
    $this->action == 'add' ? 'Přidat příspěvek' : 'Upravit příspěvek'
) ?>
<div class="container full">
<?= $this->notice($this->redirect()->getMessage()) ?>
<form action="" method="POST">
<table>
    <tr>
        <td>Nadpis:</td>
        <td><?= $this->text('nadpis', $this->nadpis) ?></td>
    </tr><tr>
        <td>Text:</td>
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
        <td>Uzamknout:</td>
        <td><?= $this->checkbox('lock')->set($this->lock) ?></td>
    </tr><tr>
        <td>Skupiny:</td>
        <td>
            <?php
            foreach ($this->skupiny as $item) {
                echo $this->checkbox('sk-' . $item['s_id'], '1')
                          ->set($this->skupinySelected[$item['s_id']])
                          ->label($this->colorbox($item['s_color_rgb'], $item['s_description'])
                                  . '&nbsp;' . $item['s_name']);
                echo '<br>';
            }
            ?>
            <?= $this->hidden('referer', $this->referer) ?>
        </td>
    </tr><tr>
        <td><?= $this->submit($this->action == 'add' ? 'Přidat' : 'Upravit') ?></td>
        <td>Pole označená (*) jsou povinná</td>
    </tr>
</table>
</form>

<a href="<?= $this->returnURI?>">Zpět</a>
</div>
