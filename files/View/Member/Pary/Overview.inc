<?= $this->header('Žebříček párů') ?>

<div class="container full">
  <table>
    <tr>
      <th>Pořadí</th>
      <th>Partner</th>
      <th>Partnerka</th>
      <th>Standard</th>
      <th>Latina</th>
      <th>Body</th>
    </tr>

    <?php $i = 1; foreach ($this->data as $item): ?>
      <tr>
        <td style="text-align:right;"><?php echo $i++;?>. </td>
        <td><?php echo $item['partnerName'];?></td>
        <td><?php echo $item['partnerkaName'];?></td>
        <td><?php echo $item['latina'];?></td>
        <td><?php echo $item['standart'];?></td>
        <td><?php echo $item['hodnoceni'];?></td>
      </tr>
    <?php endforeach;?>
  </table>
</div>
