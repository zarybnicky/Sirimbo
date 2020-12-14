<div class="container">
  <form action="" method="POST">
    <button type="submit" name="action" value="enter" style="display:none;">Enter</button>
    <div class="r_wrapper">
      <?= new \RenderHelper('files/View/Member/Rozpis/Header.inc', $this->data) ?>
      <table>
        <tr>
          <td>Tanečník</td>
          <td>Od</td>
          <td>Do</td>
          <td></td>
          <td>Zámek</td>
        </tr>
        <?php
        $userselect = (new UserSelectHelper($this->users))->idVar('p_id')->type('par');
        foreach ($this->items as $item) :
        ?>
          <tr>
            <td><?= $userselect->name($item['id'] . '-partner')->set($item['partner']) ?></td>
            <td><?= (new \TextHelper($item['id'] . '-od', $item['timeFrom']))->size(5) ?></td>
            <td><?= (new \TextHelper($item['id'] . '-do', $item['timeTo']))->size(5) ?></td>
            <td><?= (new \SubmitHelper('Odstranit'))->data('remove', $item['id']) ?></td>
            <td><?= new \CheckboxHelper($item['id'] . '-lock', 'lock', $item['lock']) ?></td>
          </tr>
        <?php endforeach ?>
        <tr>
          <td><?= $userselect->name('add_partner')->set(null) ?></td>
          <td><?= (new \TextHelper('add_od', ''))->size(5) ?>
          <td><?= (new \TextHelper('add_do', ''))->size(5) ?>
          <td><?= (new \SubmitHelper('Přidat'))->data('action', 'add') ?></td>
          <td><?= new \CheckboxHelper('add_lock', 'lock') ?>
        </tr>
      </table>
    </div>
    <div>
      <button type="submit" name="action" value="enter" style="display:none;">Enter</button>
      <?= (new \SubmitHelper('Uložit'))->data('action', 'submit') ?>
      <?= (new \SubmitHelper('Opravit přesahy'))->data('action', 'overlap') ?>
      <br />
      <?= (new \SubmitHelper('Přidat více'))->data('action', 'add_multiple') ?>
      <?= (new \TextHelper('add_multi_num', ''))->size(2) ?> hodin po
      <?= (new \TextHelper('add_multi_len', ''))->size(3) ?> minutách od
      <?= (new \TextHelper('add_multi_od', ''))->size(5) ?> hodin
    </div>
  </form>
  <br/>
  <h2>Nabídky</h2>
  <form action="" method="get">
    <?= new \SelectHelper('n', array_merge(['' => '---'], $this->nabidky), $this->selected_nabidka) ?>
    <?= new \SubmitHelper('Vybrat nabídku') ?>
    <?php if ($this->nabidka) : ?>
      <div class="n_wrapper">
        <?= new \RenderHelper('files/View/Member/Nabidka/Header.inc', $this->nabidka) ?>
        <table>
          <tr>
            <td>Tanečník</td>
            <td>Počet hodin</td>
          </tr>
          <?php foreach ($this->nabidka['items'] as $item) : ?>
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
