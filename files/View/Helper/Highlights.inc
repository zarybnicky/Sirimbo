<div class="highlights">
  <ul>
    <?php foreach ($this->data as $item): ?>
    <li>
      <a class="no-a" href="<?php echo $item['uri'] ?>">
        <div class="highlights-image">
          <img alt="" src="<?php echo $item['title_photo_uri'] ?>" />
        </div>
        <div class="highlights-info">
          <div class="category"><?php echo $item['category']; ?></div>
          <div class="name"><?php echo $item['name']; ?></div>
          <p class="desc"><?php echo $item['description']; ?></p>
        </div>
      </a>
    </li>
    <?php endforeach; ?>
  </ul>
</div>
