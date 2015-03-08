<div class="trenink-header">
    <div class="title"><?= $this->jmeno;?></div>
    <div class="date"><?= $this->datum;?></div>
    <div class="emph"><?= $this->kde;?></div>
    <div>
        <span class="big">Kapacita: </span>
        <?= $this->kapacita ?>
        <br/>
        <span class="big">Volných míst: </span>
        <?= $this->volno ?>
    </div>
    <?php if ($this->canEdit): ?>
        <span class="big">Admin: </span>
        <a href="/admin/akce/edit/<?= $this->id;?>">obecné</a>
        <a href="/admin/akce/detail/<?= $this->id;?>">účastníci</a>
        <a href="/admin/akce/dokumenty/<?= $this->id;?>">dokumenty</a>
    <?php endif; ?>
    <?php if (isset($this->info)): ?>
        <div style="text-align:left;">
            <span class="big">Další informace: </span>
            <br/><?= $this->info;?>
        </div>
    <?php endif;?>
    <?php if (isset($this->dokumenty) && $this->dokumenty): ?>
        <br/>
        <div style="text-align:left;">
            <span class="big">Dokumenty: </span><br/>
            <?php foreach ($this->dokumenty as $row): ?>
                <a href="/member/download?id=<?= $row['id'];?>">
                    <?= $row['name'];?>
                </a>
            <?php endforeach; ?>
        </div>
    <?php endif; ?>
</div>
