<div class="widget">
  <div class="widget-title">Více článků</div>
  <div id="more-articles">
    <?php foreach ($this->data as $item): ?>
    <div class="container-third">
      <div class="article-small">
        <a class="no-a" href="<?php echo $item['uri'] ?>">
          <div class="image">
            <img alt="" src="<?php echo $item['title_photo_uri'] ?>" />
          </div>
          <div class="info">
            <div class="category"><?php echo $item['category']; ?></div>
            <div class="name"><?php echo $item['name']; ?></div>
            <p class="desc"><?php echo $item['description']; ?></p>
            <div style="text-align:right;font-size:85%;">
              <a href="#">více zde &raquo;</a>
            </div>
          </div>
        </a>
      </div>
    </div>
    <?php endforeach; ?>
  </div>
</div>
