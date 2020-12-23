<div class="container">
  <table>
    <tr>
      <th>Pořadí</th>
      <th>Partner</th>
      <th>Partnerka</th>
      <th>Standard</th>
      <th>Latina</th>
      <th>Body</th>
    </tr>

    <?php $i = 1; foreach ($data as $item): ?>
      <tr>
        <td style="text-align:right;"><?= $i++ ?>. </td>
        <td><?= $item['partnerName'] ?></td>
        <td><?= $item['partnerkaName'] ?></td>
        <td><?= $item['latina'] ?></td>
        <td><?= $item['standard'] ?></td>
        <td><?= $item['hodnoceni'] ?></td>
      </tr>
    <?php endforeach ?>
  </table>
</div>
