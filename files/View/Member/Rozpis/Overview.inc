<?= $this->header('Rozpis tréninků') ?>

<div class="container full">
    <?= $this->notice($this->redirect()->getMessage()); ?>
</div>

<table class="nocolor" style="margin: 0 auto">
    <?php foreach (array_chunk($this->data, 2) as $row) : ?>
        <tr>
            <?php foreach ($row as $item): ?>
                <td style="vertical-align:top">
                    <div class="widget">
                        <div class="widget-title center">
                            <?= $this->render('files/View/Member/Rozpis/Header.inc', $item); ?>
                        </div>
                        <table class="widget-content nocolor" style="width:100%">
                            <?php foreach ($item['items'] as $lesson): ?>
                                <tr>
                                    <td style="text-align:center">
                                        <?= $lesson['timeFrom']; ?> -
                                        <?= $lesson['timeTo']; ?>
                                    </td>
                                    <td>
                                        <?= $lesson['fullName']; ?>
                                    </td>
                                    <td class="center big" style="height:20px;width:20px;">
                                        <form action="" method="post">
                                            <input type="hidden"
                                                   name="ri_id"
                                                   value="<?= $lesson['id'] ?>" />
                                            <?php if ($lesson['canReserve']): ?>
                                                <?= $this->submit('+')
                                                         ->data('action', 'signup') ?>
                                            <?php elseif ($lesson['canCancel']): ?>
                                                <?= $this->submit('&times;')
                                                         ->data('action', 'signout') ?>
                                            <?php endif; ?>
                                        </form>
                                    </td>
                                </tr>
                            <?php endforeach; ?>
                        </table>
                    </div>
                </td>
            <?php endforeach; ?>
        </tr>
    <?php endforeach; ?>
</table>
