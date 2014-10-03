<div class="container">
  <h1>Správa rozpisů</h1>
  <?php echo $this->notice($this->redirect()->getMessage()); ?>
  <form action="" method="POST">
    <button type="submit" name="action" value="enter" style="display:none;">Enter</button>
    <div class="r_wrapper">
      <?php echo $this->render('files/View/Member/Rozpis/Header.inc', $this->data); ?>
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
          post($item['id'] . '-partner', $item['partner']);
        ?>
          <tr>
            <td>
              <?php echo $userselect->name($item['id'] . '-partner'); ?>
            </td>
            <td>
              <input type="text"
                     size="5"
                     name="<?php echo $item['id']; ?>-od"
                     value="<?php echo $item['timeFrom']; ?>"/>
            </td>
            <td>
              <input type="text"
                     size="5"
                     name="<?php echo $item['id']; ?>-do"
                     value="<?php echo $item['timeTo']; ?>"/>
            </td>
            <td>
              <button type="submit"
                      name="remove"
                      value="<?php echo $item['id']; ?>">
                Odstranit
              </button>
            </td>
            <td>
              <?php echo $this->checkbox($item['id'] . '-lock', 'lock')
                              ->defaultState($item['lock']); ?>
            </td>
          </tr>
        <?php endforeach; ?>
        <tr>
          <td>
            <?php echo $userselect->name('add_partner'); ?>
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
            <button type="submit" name="action" value="submit">Uložit</button>
          </td>
        </tr>
      </table>
    </div>
    <br/>
    <h2>Nabídky</h2>
    <?php
    $nabidky = DBNabidka::getNabidka();
    
    $s = $this->select()
              ->post()->name('n_id')
                      ->option('none', '--');
    foreach ($nabidky as $item)
      $s->option($item['n_id'],
                 formatDate($item['n_od']) . ($item['n_od'] != $item['n_do'] ?
                                                               (' - ' . formatDate($item['n_do'])) : '') .
    ': ' . $item['u_jmeno'] . ' ' . $item['u_prijmeni']);
    echo $s;
    ?>
    <button type="submit" name="action" value="select">Vybrat nabídku</button>
    <?php
    if (is_numeric(post('n_id'))) {
      $n_id = post('n_id');
      if (!($nabidka = DBNabidka::getSingleNabidka($n_id))) {
        echo 'Nabídka se zadaným ID neexistuje!';
      } else {
        echo '<div class="n_wrapper">';
        DisplayNabidka::viewNabidkaHeader(
          $nabidka,
          DBNabidka::getNabidkaItemLessons($n_id)
        );
        
        $items = DBNabidka::getNabidkaItem($n_id);
        if (!empty($items)) {
          echo '<table>';
          echo '<tr><td>Tanečník</td><td>Počet hodin</td></tr>';
          foreach ($items as $item) {
            echo '<tr>';
            echo '<td>', echoFullJmeno($item), '</td>';
            echo '<td>', $item['ni_pocet_hod'], '</td>';
            echo '</tr>';
          }
          echo '</table>';
        }
        echo '</div>';
      }
    }
    ?>
    <br/><br/>
    <h2>Pomocník</h2>
    <button type="submit" name="generate" value="overlap">Opravit přesahy</button><br/>
    <button type="submit" name="generate" value="gen_add">Přidat</button>
    <input type="text" name="gen_add_hod" size="2" /> hodin po
    <input type="text" name="gen_add_len" size="3" /> minutách od
    <input type="text" name="gen_add_od" size="5" /> hodin
  </form>
  <br/>
  <a href="/admin/rozpis">Zpět</a>
</div>
