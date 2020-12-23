<div class="trenink-header">
    <div class="title"><?= $jmeno ?></div>
    <div class="date"><?= $datum ?></div>
    <div class="emph"><?= $kde ?></div>
    <div>
        <span class="big">Kapacita: </span>
        <?= $kapacita ?>
        <br/>
        <span class="big">Volných míst: </span>
        <?= $volno ?>
    </div>
    <?php if ($canEdit): ?>
        <span class="big">Admin: </span>
        <a href="/admin/akce/edit/<?= $id ?>">obecné</a>
        <a href="/admin/akce/detail/<?= $id ?>">účastníci</a>
        <a href="/admin/akce/dokumenty/<?= $id ?>">dokumenty</a>
    <?php endif ?>
    <?php if (isset($info)): ?>
        <div style="text-align:left;">
            <span class="big">Další informace: </span>
            <br/><?= $info ?>
        </div>
    <?php endif ?>
    <?php if (isset($dokumenty) && $dokumenty): ?>
        <br/>
        <div style="text-align:left;">
            <span class="big">Dokumenty: </span><br/>
            <?php foreach ($dokumenty as $row): ?>
                <a href="/member/download?id=<?= $row['id'] ?>">
                    <?= $row['name'] ?>
                </a>
            <?php endforeach ?>
        </div>
    <?php endif ?>
</div>
