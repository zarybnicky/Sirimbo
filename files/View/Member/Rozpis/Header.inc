<div class="trenink_header">
  <div class="nadpis"><?php echo $this->fullName; ?></div>
  <div style="letter-spacing:1px;font-weight:bold;"><?php echo $this->datum; ?></div>

  <?php if ($this->canEdit): ?>
    Admin:
    <a href="/admin/rozpis/edit/<?php echo $this->id; ?>">obecné</a>
    <a href="/admin/rozpis/detail/<?php echo $this->id; ?>">tréninky</a>
  <?php endif; ?>

  <div><?php echo $this->kde; ?></div>
</div>
