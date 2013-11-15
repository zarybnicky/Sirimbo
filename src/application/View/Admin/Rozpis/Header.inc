<div class="trenink_header">
    <div class="nadpis"><?= $this->fullName ?></div>
    <div style="letter-spacing:1px;font-weight:bold;"><?= $this->date ?></div>

    <?php if ($this->showAdmin) : ?>
        <span style="color:#572E00;font-size:115%;">Admin: </span>
        <a href="/admin/rozpis/edit/<?= $this->id ?>">obecné</a>
        <a href="/admin/rozpis/detail/<?= $this->id ?>">tréninky</a>
    <?php endif; ?>

    <div><?= $this->kde ?></div>
</div>
