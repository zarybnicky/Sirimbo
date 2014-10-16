<div class="container">
  <h1>Žebříček párů</h1>
  <?php echo $this->notice($this->redirect()->getMessage());?>

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
        <!--<td><a href="/member/pary/<php echo $item['id'];?>">Profil</a></td>-->
      </tr>
    <?php endforeach;?>
  </table>
</div>
