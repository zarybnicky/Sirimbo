<div class="container full">
    <?php foreach ($this->data as $item): ?>
        <div class="widget">
            <div class="widget-title"><?= $item['name'] ?></div>
            <div class="ui video" data-source="youtube"
                 data-id="<?= $item['uri'] ?>">
                </div>
        </div>
    <?php endforeach; ?>
    <script>$('.ui.video').video();</script>
</div>
