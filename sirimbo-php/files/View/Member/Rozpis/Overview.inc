<div class="container">
  <div class="row flex-wrap flex-space-around">
    <?php foreach ($data as $item): ?>
      <div class="col-12 col-md-6 col-lg-4 pb-2">
        <div class="widget">
          <div class="widget-title center">
            <?php \Render::fragment('files/View/Member/Rozpis/Header.inc', $item) ?>
          </div>
          <div class="widget-content">
            <table class="nocolor mx-auto">
              <?php foreach ($item['items'] as $lesson): ?>
                <tr>
                  <td>
                    <form action="" method="post">
                      <span class="pr-1">
                        <?= $lesson['timeFrom'] ?>-<?= $lesson['timeTo'] ?>
                      </span>
                      <?= $lesson['fullName'] ?>
                      <?= \Utils::hidden('ri_id', $lesson['id']) ?>
                      <?php if ($lesson['canReserve']): ?>
                        <?= \Utils::submit('+', 'action', 'signup', 'py-0 btn btn-outline-primary btn-sm') ?>
                      <?php elseif ($lesson['canCancel']): ?>
                        <?= \Utils::submit('&times;', 'action', 'signout', 'py-0 btn btn-outline-primary btn-sm') ?>
                      <?php endif ?>
                    </form>
                  </td>
                </tr>
              <?php endforeach ?>
            </table>
          </div>
        </div>
      </div>
    <?php endforeach ?>
  </div>
</div>
