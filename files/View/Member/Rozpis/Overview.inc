<div class="container">
  <div class="row flex-wrap flex-space-around">
    <?php foreach ($this->data as $item): ?>
      <div class="col-12 col-md-6 col-lg-4 pb-2">
        <div class="widget">
          <div class="widget-title center">
            <?= $this->render('files/View/Member/Rozpis/Header.inc', $item) ?>
          </div>
          <table class="widget-content nocolor" style="width:100%">
            <?php foreach ($item['items'] as $lesson): ?>
              <tr>
                <td><?= $lesson['timeFrom'] ?>-<?= $lesson['timeTo'] ?></td>
                <td><?= $lesson['fullName'] ?></td>
                <td class="text-center" style="width:45px;">
                  <form action="" method="post">
                    <?= $this->hidden('ri_id', $lesson['id']) ?>
                    <?php if ($lesson['canReserve']): ?>
                      <?= $this->submit('+')->cls('btn btn-outline-primary btn-sm')->data('action', 'signup') ?>
                    <?php elseif ($lesson['canCancel']): ?>
                      <?= $this->submit('&times;')->cls('btn btn-outline-primary btn-sm')->data('action', 'signout') ?>
                    <?php endif ?>
                  </form>
                </td>
              </tr>
            <?php endforeach ?>
          </table>
        </div>
      </div>
    <?php endforeach ?>
  </div>
</div>
