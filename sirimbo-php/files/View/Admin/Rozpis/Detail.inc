<div class="container">
  <form action="" method="POST">
    <button type="submit" name="action" value="enter" style="display:none;">Enter</button>
    <div class="r_wrapper">
      <?php \Render::fragment('files/View/Member/Rozpis/Header.inc', $data) ?>
      <table>
        <tr>
          <td>Tanečník</td>
          <td>Od</td>
          <td>Do</td>
          <td></td>
          <td>Zámek</td>
        </tr>
        <?php foreach ($items as $item) : ?>
          <tr>
            <td><?= \Utils::userSelect($users, $item['id'] . '-partner', $item['partner'], 'p_id') ?></td>
            <td><?= \Utils::text($item['id'] . '-od', $item['timeFrom'], ['size' => 5]) ?></td>
            <td><?= \Utils::text($item['id'] . '-do', $item['timeTo'], ['size' => 5]) ?></td>
            <td><?= \Utils::submit('Odstranit', 'remove', $item['id']) ?></td>
            <td><?= \Utils::checkbox($item['id'] . '-lock', 'lock', $item['lock']) ?></td>
          </tr>
        <?php endforeach ?>
        <tr>
          <td><?= \Utils::userSelect($users, 'add_partner', null, 'p_id') ?></td>
          <td><?= \Utils::text('add_od', '', ['size' => 5]) ?>
          <td><?= \Utils::text('add_do', '', ['size' => 5]) ?>
          <td><?= \Utils::submit('Přidat', 'action', 'add') ?></td>
          <td><?= \Utils::checkbox('add_lock', 'lock') ?>
        </tr>
      </table>
    </div>
    <div>
      <button type="submit" name="action" value="enter" style="display:none;">Enter</button>
      <?= \Utils::submit('Uložit', 'action', 'submit') ?>
      <?= \Utils::submit('Opravit přesahy', 'action', 'overlap') ?>
      <br />
      <?= \Utils::submit('Přidat více', 'action', 'add_multiple') ?>
      <?= \Utils::text('add_multi_num', '', ['size' => 2]) ?> hodin po
      <?= \Utils::text('add_multi_len', '', ['size' => 3]) ?> minutách od
      <?= \Utils::text('add_multi_od', '', ['size' => 5]) ?> hodin
    </div>
  </form>
  <br/>
  <h2>Nabídky</h2>
  <form action="" method="get">
    <?= \Utils::select('n', ['' => '---'] + $nabidky, $selected_nabidka) ?>
    <?= \Utils::submit('Vybrat nabídku') ?>
    <?php if ($nabidka) : ?>
      <div class="n_wrapper">
        <?php \Render::fragment('files/View/Member/Nabidka/Header.inc', $nabidka) ?>
        <table>
          <tr>
            <td>Tanečník</td>
            <td>Počet hodin</td>
          </tr>
          <?php foreach ($nabidka['items'] as $item) : ?>
            <tr>
              <td><?= $item['fullName'] ?></td>
              <td><?= $item['lessonCount'] ?></td>
            </tr>
          <?php endforeach ?>
        </table>
      </div>
    <?php endif ?>
  </form>
  <br/>
  <a class="btn btn-outline-primary" href="/admin/rozpis">Zpět</a>
</div>
