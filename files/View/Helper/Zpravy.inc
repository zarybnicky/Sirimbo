<ul class="results scroll-y">
  <?php foreach ($this->data as $item): ?>
    <li class="competition">
      <div class="widget-content">
        <?= $item['text']; ?>
      </div>
    </li>
  <?php endforeach; ?>
</ul>
