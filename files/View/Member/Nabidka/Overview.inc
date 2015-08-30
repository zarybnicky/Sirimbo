<?= $this->header('Nabídka tréninků')?>

<div class="container full">
  <?= $this->notice($this->redirect()->getMessage());?>
</div>

<table class="nocolor" style="margin: 0 auto">
    <?php foreach (array_chunk($this->data, 2) as $chunk) : ?>
        <tr>
            <?php foreach ($chunk as $item) : ?>
                <td style="vertical-align:top">
                    <form action="" method="post">
                        <div class="widget">
                            <div class="widget-title center">
                                <?= $this->render('files/View/Member/Nabidka/Header.inc', $item);?>
                            </div>
                            <div class="widget-content">
                                <table class="nocolor" style="width:100%">
                                    <?php foreach ($item['items'] as $row) : ?>
                                        <tr>
                                            <td>
                                                <?= $row['fullName'];?>
                                            </td>
                                            <td>
                                                <?= $row['hourCount'];?>
                                                <?= (($row['hourCount'] == 1) ?
                                                      'hodina' :
                                                      (($row['hourCount'] > 1 &&
                                                        $row['hourCount'] < 4) ?
                                                        'hodiny' : 'hodin')) ?>
                                            </td>
                                            <td>
                                                <?php if ($row['canDelete']) : ?>
                                                    <?= $this->submit('&times')
                                                             ->data('un_id', $row['deleteTicket']) ?>
                                                <?php endif;?>
                                            </td>
                                        </tr>
                                    <?php endforeach;?>
                                </table>
                                <div style="text-align:center;padding:10px 0 5px">
                                    <?php if ($item['canAdd']) : ?>
                                        Počet hodin:
                                        <input type="text"
                                               name="hodiny"
                                               size="2"
                                               value="<?= $this->hodiny ?>" />
                                        <input type="hidden"
                                               name="id"
                                               value="<?= $item['id'];?>" />
                                        <?= $this->submit('Přidat') ?>
                                    <?php endif;?>
                                </div>
                            </div>
                        </div>
                    </form>
                </td>
            <?php endforeach;?>
        </tr>
    <?php endforeach;?>
</table>
