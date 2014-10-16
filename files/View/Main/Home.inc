<?php echo $this->clanky()->number(3)->highlights(true);?>

<div class="container">
  <?php echo $this->notice($this->redirect()->getMessage()); ?>

  <?php echo $this->zpravy()->number(10);?>
  <!-- Dodat "vice zde >>" na konec boxu -->

  <div class="container-third">
    <div class="widget">
      <div class="widget-title">Videa z posledních soutěží</div>
      <div class="widget-content">
        <ul class="videos">
          <li>
            <a href="https://www.youtube.com/watch?v=6nj5RT8FLa4" class="no-a">
              <img alt="" src="//i3.ytimg.com/vi/6nj5RT8FLa4/hqdefault.jpg" />
            </a>
          </li>
          <li>
            <a href="https://www.youtube.com/watch?v=szgAEUOIrmc" class="no-a">
              <img alt="" src="//i3.ytimg.com/vi/szgAEUOIrmc/hqdefault.jpg" />
            </a>
          </li>
        </ul>
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
