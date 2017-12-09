<?= $this->highlights($this->highlights);?>

<div class="container full">
  <?= $this->notice($this->redirect()->getMessage()); ?>

  <!--<div class="container third">
    <div class="widget">
      <div class="widget-title text">Výsledky posledních soutěží</div>
      < ?= $this->results($this->results);?>
    </div>
  </div>-->

  <div class="container two-thirds">
    <div class="widget">
      <div class="widget-title text">Videa z posledních soutěží</div>
      <div class="widget-content">
          <?= $this->videos ?>
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
