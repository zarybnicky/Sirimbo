<div class="container third">
  <div class="widget">
    <div class="widget-title">Výsledky posledních soutěží</div>
    <ul class="results scroll-y">
    <?php foreach ($this->data as $item): ?>
      <li class="competition">
        <div class="widget-content">
          <?php echo $item['text']; ?>
        </div>
      </li>
    <?php endforeach; ?>
    </ul>
  </div>
</div>
