<?php echo $this->render('src/application/View/Member/Akce/Header.inc', $this->data);?>

<div style="text-align:center;">

<?php
if (empty($this->data['items'])) : ?>
    Nikdo není přihlášený<br/>
<?php
else: ?>
    <table style="margin:0 auto;">
        <tr>
            <td>Jméno</td>
            <td>Rok narození</td>
        </tr>
    <?php
    foreach ($this->data['items'] as $item) : ?>
        <tr>
            <td><?php echo $item['u_jmeno'], ' ', $item['u_prijmeni'];?></td>
            <td><?php echo $item['ai_rok_narozeni'];?></td>
        </tr>
    <?php
    endforeach; ?>
    </table>
<?php
endif; ?>
<a href="/member/akce">Zpět</a>
</div>