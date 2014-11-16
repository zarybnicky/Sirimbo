<div class="trenink-header">
  <div class="title"><?= $this->fullName; ?></div>
  <div class="date"><?= $this->datum; ?></div>
  <div><?= $this->kde; ?></div>

  <?php if ($this->canEdit): ?>
    Admin:
    <a href="/admin/rozpis/edit/<?= $this->id; ?>">obecné</a>
    <a href="/admin/rozpis/detail/<?= $this->id; ?>">tréninky</a>
  <?php endif; ?>
</div>
