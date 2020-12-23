<?php \Render::fragment('files/View/Admin/Platby/Navigation.inc') ?>

<div class="container">
  <a style="display:block;margin-bottom:8px;" href="?list=all">Všechny platby</a>
  <?php if (count($groupByDate) > 0): ?>
    <span class="big">Platby podle rozpoznaného data:</span>
    <ul style="list-style:disc outside;">
      <?php
      foreach ($groupByDate as $yearValue => $year) {
          $href = '?list=date&year=' . $yearValue;
          echo '<li style="margin-left:1em"><a href="', $href, '">', $year['name'], '</a></li>';

          if (!isset($year['months']))
              continue;

          echo '<ul style="list-style:disc outside;margin-left:1em;">';
          foreach ($year['months'] as $monthValue => $month) {
              echo '<li style="margin-left:1em"><a href="', $href, '&month=', $monthValue, '">', $month, '</a>';
          }
          echo '</li></ul>';
      }
      ?>
      </ul>
      <br/>
  <?php endif ?>
  <?php if (count($groupByAmount) > 0): ?>
    <span class="big">Platby podle rozpoznané částky:</span>
    <ul style="list-style:disc outside;">
      <?php foreach ($groupByAmount as $value => $name): ?>
        <li style="margin-left:1em">
          <a href="?list=amount&amount=<?= $value ?>"><?= $name ?></a>
        </li>
      <?php endforeach ?>
    </ul>
  <?php endif ?>
</div>
