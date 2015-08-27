<?= $this->header('Správa videí') ?>
<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()) ?>
    <form action="" method="post">
        <div class="r">
            <a href="/admin/video/add">Přidat</a>
            <button name="action" value="remove">Odebrat</button>
        </div>
        <?php if ($this->data): ?>
            <?= $this->table()
                     ->style('width:100%')
                     ->data($this->data)
                     ->column('checkBox', '')
                     ->column('name', 'Jméno')
                     ->column('uri', 'URI')
                     ->column('playlist', 'Playlist?')
                     ->column('date', 'Datum přidání')
                     ->column('links', 'Upravit') ?>
        <?php else: ?>
            <?= $this->notice('Žádná videa') ?>
        <?php endif ?>
    </form>
</div>
