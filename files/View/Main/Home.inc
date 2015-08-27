<?= $this->highlights($this->highlights);?>

<div class="container full">
  <?= $this->notice($this->redirect()->getMessage()); ?>

  <div class="container third">
    <div class="widget">
      <div class="widget-title text">Výsledky posledních soutěží</div>
      <?= $this->results($this->results);?>
    </div>
  </div>

  <div class="container third">
    <div class="widget">
      <div class="widget-title text">Videa z posledních soutěží</div>
      <div class="widget-content">
        <ul class="videos">
          <li>
            <a href="https://www.youtube.com/watch?v=6nj5RT8FLa4" target="_blank" class="no-a">
              <img alt="" src="//i3.ytimg.com/vi/6nj5RT8FLa4/hqdefault.jpg" />
            </a>
          </li>
          <li>
            <a href="https://www.youtube.com/watch?v=szgAEUOIrmc" target="_blank" class="no-a">
              <img alt="" src="//i3.ytimg.com/vi/szgAEUOIrmc/hqdefault.jpg" />
            </a>
          </li>
        </ul>
      </div>
    </div>
  </div>

  <div class="container third">
    <div class="widget">
      <div class="widget-title text">O klubu</div>
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
<div class="container full">
  <?= $this->articles($this->moreArticles);?>
</div>
