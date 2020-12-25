<div class="container">
  <div class="row flex-wrap flex-space-around">
    <?php foreach ($data as $item): ?>
      <div class="col-12 col-md-6 col-lg-4 pb-2">
        <form action="" method="post">
          <div class="widget">
            <div class="widget-title center">
              <?php \Render::fragment('files/View/Member/Nabidka/Header.inc', $item) ?>
            </div>
            <div class="widget-content">
              <table class="nocolor" style="width:100%">
                <?php foreach ($item['items'] as $row) : ?>
                  <tr>
                    <td><?= $row['fullName'] ?></td>
                    <td>
                      <?= $row['hourCount'] ?>
                      <?= (($row['hourCount'] == 1) ?
                          'hodina' :
                          (($row['hourCount'] > 1 &&
                            $row['hourCount'] < 4) ?
                           'hodiny' : 'hodin')) ?>
                    </td>
                    <td>
                      <?php if ($row['canDelete']) : ?>
                        <?= \Utils::submit('&times', 'un_id', $row['deleteTicket'], 'py-0 btn btn-outline-primary btn-sm') ?>
                      <?php endif ?>
                    </td>
                  </tr>
                <?php endforeach ?>
              </table>
              <div style="text-align:center;padding:10px 0 5px">
                <?php if ($item['canAdd']) : ?>
                  <?= \Utils::text('hodiny', '', ['placeholder' => 'Počet hodin', 'cls' => 'form-control d-inline w-auto']) ?>
                  <input type='hidden' name='id' value='<?= $item['id'] ?>'>
                  <?= \Utils::submit('Přidat') ?>
                <?php endif ?>
              </div>
            </div>
          </div>
        </form>
      </div>
    <?php endforeach ?>
  </div>
</div>
