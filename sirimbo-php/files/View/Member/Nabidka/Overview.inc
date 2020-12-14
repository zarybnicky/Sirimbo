<div class="container">
  <div class="row flex-wrap flex-space-around">
    <?php foreach ($this->data as $item): ?>
      <div class="col-12 col-md-6 col-lg-4 pb-2">
        <form action="" method="post">
          <div class="widget">
            <div class="widget-title center">
              <?php new \RenderHelper('files/View/Member/Nabidka/Header.inc', $item) ?>
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
                        <?= (new \SubmitHelper('&times'))->cls('py-0 btn btn-outline-primary btn-sm')
                               ->data('un_id', $row['deleteTicket']) ?>
                      <?php endif ?>
                    </td>
                  </tr>
                <?php endforeach ?>
              </table>
              <div style="text-align:center;padding:10px 0 5px">
                <?php if ($item['canAdd']) : ?>
                  <?= (new \TextHelper('hodiny', ''))->placeholder('Počet hodin')->cls('form-control d-inline w-auto') ?>
                  <?= new \HiddenHelper('id', $item['id']) ?>
                  <?= new \SubmitHelper('Přidat') ?>
                <?php endif ?>
              </div>
            </div>
          </div>
        </form>
      </div>
    <?php endforeach ?>
  </div>
</div>
