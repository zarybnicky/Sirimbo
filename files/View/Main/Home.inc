<?php echo $this->clanky()->number(5)->highlights(true);?>

<div class="container">
  <?php notice($this->redirect()->getMessage()); ?>

  <?php echo $this->zpravy()->number(10);?>
  <!-- Dodat "vice zde >>" na konec boxu -->

  <div class="container-third">
    <div class="widget">
      <div class="widget-title">Videa z posledních soutěží</div>
      <div class="widget-content">
        Videa...
      </div>
    </div>
  </div>

  <div class="container-third">
    <div class="widget">
      <div class="widget-title">O klubu</div>
      <div class="widget-content">
        <p>
          <b>TK Olymp Olomouc</b> je tradiční taneční klub zabývající se výchovou
          standardních a latinskoamerických tanečníků. V novém tisíciletí
          patří mezi nejúspěšnější české kluby. Naše páry pravidelně sbírají
          úspěchy v České republice i na mezinárodním poli.
        </p>
        <p>
          Nabízíme atraktivní volnočasovou aktivitu dětem, kvalitní zázemí
          sportovním tanečníkům a příjemné prostředí pro všechny, co mají
          rádi tanec.
        </p>
        <div style="text-align:right;font-size:85%;">
          <a href="#">více zde &raquo;</a>
        </div>
      </div>
    </div>
  </div>
</div>
<div class="container">
  <?php echo $this->clanky()->number(9)->offset(5)->highlights(false);?>
</div>
