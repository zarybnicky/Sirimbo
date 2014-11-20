<?= $this->header($this->nadpis) ?>

<div class="container full">
    <?php foreach ($this->data as $key => $row) : ?>
        <div class="article-list">
            <div class="image">
                <img src="<?= $row['title_photo_thumb_uri'] ?>" />
            </div>
            <div class="name">
                <a href="/aktualne/<?= $row['id'] ?>">
                    <?= $row['jmeno'] ?>
                </a>
            </div>
            <div class="info little">
                <?= formatTimestamp($row['timestamp'])?>
                <?php if ($row['canEdit']) : ?>
                    - <a href="/admin/aktuality/edit/<?= $row['id'] ?>">Upravit</a>
                <?php endif;?>
            </div>
            <div class="desc">
                <div>
                    <?= $row['preview'] ?>
                </div>
                <div class="more">
                    <a href="/aktualne/<?= $row['id']?>">
                        více zde &raquo;
                    </a>
                </div>
            </div>
        </div>
    <?php endforeach;?>
</div>
