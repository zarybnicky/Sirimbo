<div class="container">
  <?= \Utils::partnerRequest() ?>

  <form action="" method="post">
    <table>
      <tr><td colspan="2">Standard:</td></tr>
      <tr>
        <td>Třída:</td>
        <td><?= \Utils::selectLiteral('stt-trida', ['Z', 'H', 'D', 'C', 'B', 'A', 'M'], $stt_trida) ?></td>
      </tr>
      <tr>
        <td>Body:</td>
        <td><?= \Utils::text('stt-body', $stt_body) ?></td>
      </tr>
      <tr>
        <td>Finále:</td>
        <td><?= \Utils::text('stt-finale', $stt_finale) ?></td>
      </tr>
      <tr><td colspan="2">Latina:</td></tr>
      <tr>
        <td>Třída:</td>
        <td><?= \Utils::selectLiteral('lat-trida', ['Z', 'H', 'D', 'C', 'B', 'A', 'M'], $lat_trida) ?></td>
      </tr>
      <tr>
        <td>Body:</td>
        <td><?= \Utils::text('lat-body', $lat_body) ?></td>
      </tr>
      <tr>
        <td>Finále:</td>
        <td><?= \Utils::text('lat-finale', $lat_finale) ?></td>
      </tr>
    </table>
    <?= \Utils::submit('Změnit třídu a body') ?>
    <a class="btn btn-outline-primary" href="/member/profil">Zpět</a>
  </form>
</div>
