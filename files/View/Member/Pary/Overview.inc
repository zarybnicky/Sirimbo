<h1>Žebříček párů</h1>
<?php echo $this->notice($this->redirect()->getMessage());?>

<table>
<tr>
    <td>Pořadí</td>
    <td>Partner</td>
    <td>Partnerka</td>
    <td>Standard</td>
    <td>Latina</td>
    <td>Body</td>
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
