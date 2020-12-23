<div class="trenink-header">
  <div class="title"><?= $fullName ?></div>
  <div class="date"><?= $datum ?></div>
  <div><?= $kde ?></div>

  <?php if ($canEdit): ?>
    Admin:
    <a href="/admin/rozpis/edit/<?= $id ?>">obecné</a>
    <a href="/admin/rozpis/detail/<?= $id ?>">tréninky</a>
  <?php endif ?>
</div>
