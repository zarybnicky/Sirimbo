<h1>Rozpis tréninků</h1>
<?php notice($this->redirect()->getMessage()); ?>

<style type="text/css">
.unit {background:inherit;border:none;
    vertical-align:top;border-bottom:1px dotted #999;padding:10px 5px;}';
</style>
<table style="width:100%;">
<?php foreach (array_chunk($this->data, 2) as $chunk) : ?>
    <tr>
    <?php foreach ($chunk as $item) : ?>
        <td class="unit">
        <form action="" method="post">
        <?php echo $this->render('src/application/View/Member/Nabidka/Header.inc', $item);?>
        <table style="margin:0 auto;">
            <tr>
                <td>Od</td>
                <td>Do</td>
                <td>Tanečník</td>
            </tr>
            <?php foreach ($item['items'] as $row) : ?>
            <tr>
                <td><?= $row['od'] ?></td>
                <td><?= $row['do'] ?></td>
                <td>
                    <?= $roe['fullName'] ?>
                    <input type="hidden" name="ri_id" value="<?= $row[''] ?>" />
                </td>

                <?php if ($row['showSignup']): ?>
                <td>
                    <button type="submit" name="action" value="signup">Rezervovat</button>
                </td>
                <?php endif; ?>

                <?php if ($row['showRemove']): ?>
                <td>
                    <button type="submit" name="action" value="signout">Zrušit</button>
                </td>
                <?php endif; ?>
            </tr>
            <?php endforeach; ?>
        </table>
        </form>
        </td>
    <?php endforeach; ?>
    </tr>
<?php endforeach; ?>
</table>