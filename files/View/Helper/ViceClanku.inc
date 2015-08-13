<div class="widget">
  <div class="widget-title text">Více článků</div>
  <div id="more-articles">
    <div class="container third grid-sizer"></div>
    <?php foreach ($this->data as $item): ?>
      <div class="container third item">
        <div class="article-small">
          <a class="no-a image" href="<?= $item['uri'] ?>">
            <img alt="" src="<?= $item['title_photo_uri'] ?>" />
          </a>
          <div class="info">
            <div class="category"><?= $item['category']; ?></div>
            <div class="name">
              <a href="<?= $item['uri'] ?>">
                <?= $item['name']; ?>
              </a>
            </div>
            <p class="desc"><?= $item['description']; ?></p>
            <div style="text-align:right;font-size:85%;">
              <a href="<?= $item['uri'] ?>">
                více zde &raquo;
              </a>
            </div>
          </div>
        </div>
      </div>
    <?php endforeach; ?>
  </div>
</div>
