<div class="highlights">
  <ul>
    <?php foreach ($this->data as $item): ?>
    <li>
      <a class="no-a" href="<?= $item['uri'] ?>">
        <div class="highlights-image">
          <img alt="" src="<?= $item['title_photo_uri'] ?>" />
        </div>
        <div class="highlights-info">
          <div class="category"><?= $item['category']; ?></div>
          <div class="name"><?= $item['name']; ?></div>
          <p class="desc"><?= $item['description']; ?></p>
        </div>
      </a>
    </li>
    <?php endforeach; ?>
  </ul>
</div>
