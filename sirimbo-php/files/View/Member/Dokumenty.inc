<div class="container">
    <form action="" method="GET">
      <?= \Utils::select('kat', $categories, $kat) ?>
      <?= \Utils::submit('Zobrazit') ?>
    </form>

  <table>
    <tr>
      <th>Jméno souboru</th>
      <th>Soubor</th>
      <th>Kategorie</th>
      <th>Uploadoval</th>
    </tr>
    <?php foreach ($data as $item): ?>
      <tr>
        <td><?= $item['name'] ?></td>
        <td><a href="/member/download?id=<?= $item['id'] ?>">
          <?= $item['fileName'] ?>
        </a></td>
        <td><?= $item['kategorie'] ?></td>
        <td><?= $item['uploadedBy'] ?></td>
      </tr>
    <?php endforeach ?>
  </table>
</div>
