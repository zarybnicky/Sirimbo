<?= $this->header('Správa rozpisů');?>
<div class="container full">
  <?= $this->notice($this->redirect()->getMessage()); ?>
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
              <button type="submit"
                      name="remove"
                      value="<?= $item['id']; ?>">
                Odstranit
              </button>
            </td>
            <td>
              <?= $this->checkbox($item['id'] . '-lock', 'lock')
                              ->set($item['lock']); ?>
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
            <button type="submit" name="action" value="add">Přidat</button>
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
      <button type="submit" name="action" value="submit">Uložit</button>
      <button type="submit" name="action" value="enter" style="display:none;">Enter</button>
      <button type="submit" name="action" value="overlap">Opravit přesahy</button><br/>
      <button type="submit" name="action" value="add_multiple">Přidat více:</button>
      <input type="text" name="add_multi_num" size="2" /> hodin po
      <input type="text" name="add_multi_len" size="3" /> minutách od
      <input type="text" name="add_multi_od" size="5" /> hodin
    </div>
  </form>
  <br/>
  <h2>Nabídky</h2>
  <form action="" method="get">
    <?php
    echo $this->select()
              ->get()
              ->name('n')
              ->option('', '--')
              ->set($this->selected_nabidka)
              ->options($this->nabidky);
    ?>
    <button type="submit">Vybrat nabídku</button>
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
  <a href="/admin/rozpis">Zpět</a>
</div>
