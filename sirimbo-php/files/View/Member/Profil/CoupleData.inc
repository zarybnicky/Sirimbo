<div class="container">
  <?= (new PartnerRequestHelper())->getAll() ?>

  <form action="" method="post">
    <table>
      <tr><td colspan="2">Standard:</td></tr>
      <tr>
        <td>Třída:</td>
        <td><?= $this->select('stt-trida')
                  ->set($this->stt_trida)
                  ->options(['Z', 'H', 'D', 'C', 'B', 'A', 'M'], true, true) ?></td>
      </tr>
      <tr>
        <td>Body:</td>
        <td><?= new \TextHelper('stt-body', $this->stt_body) ?></td>
      </tr>
      <tr>
        <td>Finále:</td>
        <td><?= new \TextHelper('stt-finale', $this->stt_finale) ?></td>
      </tr>
      <tr><td colspan="2">Latina:</td></tr>
      <tr>
        <td>Třída:</td>
        <td><?= $this->select('lat-trida')
                  ->set($this->lat_trida)
                  ->options(['Z', 'H', 'D', 'C', 'B', 'A', 'M'], true, true) ?></td>
      </tr>
      <tr>
        <td>Body:</td>
        <td><?= new \TextHelper('lat-body', $this->lat_body) ?></td>
      </tr>
      <tr>
        <td>Finále:</td>
        <td><?= new \TextHelper('lat-finale', $this->lat_finale) ?></td>
      </tr>
    </table>
    <?= $this->submit('Změnit třídu a body') ?>
    <a class="btn btn-outline-primary" href="/member/profil/par">Zpět</a>
  </form>
</div>
