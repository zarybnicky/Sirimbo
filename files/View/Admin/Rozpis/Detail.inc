<?= $this->header('Správa rozpisů');?>

<div class="container full">
  <form action="" method="POST">
    <button type="submit" name="action" value="enter" style="display:none;">Enter</button>
    <div class="r_wrapper">
      <?= $this->render('files/View/Member/Rozpis/Header.inc', $this->data); ?>
      <table>
        <tr>
          <td>Tanečník</td>
          <td>Od</td>
          <td>Do</td>
          <td></td>
          <td>Zámek</td>
        </tr>
        <?php
        $userselect = $this->userSelect()
                           ->idVar('p_id')
                           ->type('par')
                           ->users($this->users);
        foreach ($this->items as $item) :
        ?>
          <tr>
            <td>
              <?= $userselect->name($item['id'] . '-partner')->set($item['partner']) ?>
            </td>
            <td>
              <input type="text"
                     size="5"
                     name="<?= $item['id']; ?>-od"
                     value="<?= $item['timeFrom']; ?>"/>
            </td>
            <td>
              <input type="text"
                     size="5"
                     name="<?= $item['id']; ?>-do"
                     value="<?= $item['timeTo']; ?>"/>
            </td>
            <td>
              <?= $this->submit('Odstranit')->data('remove', $item['id']) ?>
            </td>
            <td>
              <?= $this->checkbox($item['id'] . '-lock', 'lock')->set($item['lock']) ?>
            </td>
          </tr>
        <?php endforeach; ?>
        <tr>
          <td>
            <?= $userselect->name('add_partner'); ?>
          </td>
          <td>
            <input type="text" name="add_od" size="5" value="" />
          </td>
          <td>
            <input type="text" name="add_do" size="5" value="" />
          </td>
          <td>
            <?= $this->submit('Přidat')->data('action', 'add') ?>
          </td>
          <td>
            <input type="checkbox" name="add_lock" value="lock" />
          </td>
        </tr>
        <tr>
          <td>
          </td>
        </tr>
      </table>
    </div>
    <div>
      <button type="submit" name="action" value="enter" style="display:none;">Enter</button>
      <?= $this->submit('Uložit')->data('action', 'submit') ?>
      <?= $this->submit('Opravit přesahy')->data('action', 'overlap') ?>
      <br />
      <?= $this->submit('Přidat více')->data('action', 'add_multiple') ?>
      <input type="text" name="add_multi_num" size="2" /> hodin po
      <input type="text" name="add_multi_len" size="3" /> minutách od
      <input type="text" name="add_multi_od" size="5" /> hodin
    </div>
  </form>
  <br/>
  <h2>Nabídky</h2>
  <form action="" method="get">
    <?= $this->select('n')
             ->set($this->selected_nabidka)
             ->option('', '--')
             ->options($this->nabidky);
    ?>
    <?= $this->submit('Vybrat nabídku') ?>
    <?php if ($this->nabidka) : ?>
      <div class="n_wrapper">
        <?= $this->render('files/View/Member/Nabidka/Header.inc', $this->nabidka); ?>
        <table>
          <tr>
            <td>Tanečník</td>
            <td>Počet hodin</td>
          </tr>
          <?php foreach ($this->nabidka['items'] as $item) : ?>
            <tr>
              <td><?= $item['fullName']; ?></td>
              <td><?= $item['lessonCount']; ?></td>
            </tr>
          <?php endforeach; ?>
        </table>
      </div>
    <?php endif; ?>
  </form>
  <br/>
  <a class="btn btn-outline-primary" href="/admin/rozpis">Zpět</a>
</div>
