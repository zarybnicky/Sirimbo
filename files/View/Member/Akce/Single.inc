<?= $this->header('Klubové akce') ?>

<div class="container full">
    <?php
    $data = $this->data;
    unset($data['info']);
    unset($data['dokumenty']);
    echo $this->render('files/View/Member/Akce/Header.inc', $data);
    ?>

    <div style="text-align:center;">
        <?php if (empty($this->data['items'])) : ?>
            Nikdo není přihlášený<br/>
        <?php else: ?>
            <table style="margin:0 auto;">
                <tr>
                    <td>Jméno</td>
                    <td>Rok narození</td>
                </tr>
                <?php foreach ($this->data['items'] as $item) : ?>
                    <tr>
                        <td><?php echo $item['u_jmeno'], ' ', $item['u_prijmeni'];?></td>
                        <td><?php echo $item['ai_rok_narozeni'];?></td>
                    </tr>
                <?php endforeach; ?>
            </table>
        <?php endif; ?>
        <a class="btn btn-outline-primary" href="/member/akce">Zpět</a>
    </div>
</div>
