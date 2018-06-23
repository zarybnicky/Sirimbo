<?= $this->header('Správa videí') ?>

<div class="container full">
    <form action="" method="post">
        Titulní stránka:<br>
        <?= $this->video1 ?> <?= $this->video2 ?><br>
        <?= $this->video3 ?> <?= $this->video4 ?><br>
        <?= $this->submit('Odeslat') ?>
    </form>
    <form action="" method="post">
        <div class="r">
            <a href="/admin/video/add">Přidat</a>
            <button name="action" value="remove">Odebrat</button>
        </div>
        <?php if ($this->data): ?>
            <?= $this->table()
                     ->style('width:100%')
                     ->data($this->data)
                     ->column('name', 'Jméno')
                     ->column('uri', 'URI')
                     ->column('playlist', 'Playlist?')
                     ->column('date', 'Datum přidání') ?>
        <?php else: ?>
            <?= $this->notice('Žádná videa') ?>
        <?php endif ?>
    </form>
</div>
