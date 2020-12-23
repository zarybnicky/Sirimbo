<div class="container">
  <?= new PartnerRequestHelper() ?>

  <form action="" method="post">
    <table>
      <tr><td colspan="2">Standard:</td></tr>
      <tr>
        <td>Třída:</td>
        <td><?= (new \SelectHelper('stt-trida', [], $stt_trida))
                  ->options(['Z', 'H', 'D', 'C', 'B', 'A', 'M'], true, true) ?></td>
      </tr>
      <tr>
        <td>Body:</td>
        <td><?= new \TextHelper('stt-body', $stt_body) ?></td>
      </tr>
      <tr>
        <td>Finále:</td>
        <td><?= new \TextHelper('stt-finale', $stt_finale) ?></td>
      </tr>
      <tr><td colspan="2">Latina:</td></tr>
      <tr>
        <td>Třída:</td>
        <td><?= (new \SelectHelper('lat-trida', [], $lat_trida))
                  ->options(['Z', 'H', 'D', 'C', 'B', 'A', 'M'], true, true) ?></td>
      </tr>
      <tr>
        <td>Body:</td>
        <td><?= new \TextHelper('lat-body', $lat_body) ?></td>
      </tr>
      <tr>
        <td>Finále:</td>
        <td><?= new \TextHelper('lat-finale', $lat_finale) ?></td>
      </tr>
    </table>
    <?= new \SubmitHelper('Změnit třídu a body') ?>
    <a class="btn btn-outline-primary" href="/member/profil">Zpět</a>
  </form>
</div>
