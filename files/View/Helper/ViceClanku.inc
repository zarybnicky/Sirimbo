<div class="widget">
  <div class="widget-title">Více článků</div>
  <div id="more-articles">
    <div class="container-third grid-sizer"></div>
    <?php foreach ($this->data as $item): ?>
      <div class="container-third item">
        <div class="article-small">
          <a class="no-a image" href="<?php echo $item['uri'] ?>">
            <img alt="" src="<?php echo $item['title_photo_uri'] ?>" />
          </a>
          <div class="info">
            <div class="category"><?php echo $item['category']; ?></div>
            <div class="name">
              <a href="<?php echo $item['uri'] ?>">
                <?php echo $item['name']; ?>
              </a>
            </div>
            <p class="desc"><?php echo $item['description']; ?></p>
            <div style="text-align:right;font-size:85%;">
              <a href="<?php echo $item['uri'] ?>">
                více zde &raquo;
              </a>
            </div>
          </div>
        </div>
      </div>
    <?php endforeach; ?>
  </div>
</div>