<div class="trenink_header">
    <div class="nadpis">
        <?= $this->fullName ?>
    </div>
    <div style="letter-spacing:1px;font-weight:bold;">
        <?= $this->date ?>
    </div>

    <?php if ($this->showAdmin) : ?>
    <div>
        <span style="color:#572E00;font-size:115%;">Admin: </span>
        <a href="/admin/nabidka/edit/<?= $this->id ?>">obecné</a>
        <a href="/admin/nabidka/detail/<?= $this->id ?>">tréninky</a>
    </div>
    <?php endif; ?>

    <?php if ($this->maxHodin > 0) : ?>
    <div>
        <span class="little">Maximálne hodin/pár:</span>
        <span class="nadpis"><?= $this->maxLessons ?></span>
    </div>
    <?php endif; ?>

    <div>
        <span class="little">Celkem hodin: </span>
        <span class="nadpis"><?= $this->totalLessons ?></span>
    </div>
    <div>
        <span class="little">Obsazených hodin: </span>
        <span class="nadpis"><?= $this->occupied ?></span>
    </div>
    <div>
        <span class="little">Volných hodin: </span>
        <span class="nadpis"><?= $this->totalLessons - $this->occupied ?></span>
    </div>
</div>