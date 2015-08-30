<?= $this->header('Změna třídy a bodů') ?>

<div class="container full">
  <?php
  echo $this->partnerRequest()->getAll();
  echo $this->notice($this->redirect()->getMessage());
  ?>
  <form action="" method="post">
    <table>
      <tr><td>Standart:</td><td></td><td></td></tr>
      <tr>
        <td>Třída:</td>
        <td><?php
            echo $this->select('stt-trida')
                      ->set($this->stt_trida)
                      ->options(array('Z', 'H', 'D', 'C', 'B', 'A', 'M'), true, true);
            ?></td>
        <td></td>
      </tr>
      <tr>
        <td>Body:</td>
        <td><input type="text" name="stt-body" value="<?= $this->stt_body ?>" /></td>
        <td>0 až 1000</td>
      </tr>
      <tr>
        <td>Finále:</td>
        <td><input type="text" name="stt-finale" value="<?= $this->stt_finale ?>" /></td>
        <td>0 až 10</td>
      </tr>
      <tr><td>Latina:</td><td></td><td></td></tr>
      <tr>
        <td>Třída:</td>
        <td><?php
            echo $this->select('lat-trida')
                      ->set($this->lat_trida)
                      ->options(array('Z', 'H', 'D', 'C', 'B', 'A', 'M'), true, true);
            ?></td>
        <td></td>
      </tr>
      <tr>
        <td>Body:</td>
        <td><input type="text" name="lat-body" value="<?= $this->lat_body ?>" /></td>
        <td>0 až 1000</td>
      </tr>
      <tr>
        <td>Finále:</td>
        <td><input type="text" name="lat-finale" value="<?= $this->lat_finale ?>" /></td>
        <td>0 až 10</td>
      </tr>
    </table>
    <?= $this->submit('Změnit třídu a body') ?>
    <a href="/member/profil/par">Zpět</a>
  </form>
</div>
