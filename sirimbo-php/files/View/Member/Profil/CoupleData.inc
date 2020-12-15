<div class="container">
  <?= new PartnerRequestHelper() ?>

  <form action="" method="post">
    <table>
      <tr><td colspan="2">Standard:</td></tr>
      <tr>
        <td>Třída:</td>
        <td><?= (new \SelectHelper('stt-trida', [], $this->stt_trida))
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
        <td><?= (new \SelectHelper('lat-trida', [], $this->lat_trida))
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
    <?= new \SubmitHelper('Změnit třídu a body') ?>
    <a class="btn btn-outline-primary" href="/member/profil">Zpět</a>
  </form>
</div>
