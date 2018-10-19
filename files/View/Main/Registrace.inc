<div class="container">
  <form action="" method="POST">

    <div class="form-row">
      <div class="form-group col-12 col-md-6">
        <label for="username">Přihlašovací jméno</label>
        <?= $this->text('username', $this->username) ?>
        <small class="form-text text-muted">
          Pouze písmena bez diakritiky, číslice a podtržítka, 3 - 20 znaků
        </small>
      </div>
      <div class="form-group col-12 col-md-6">
        <label for="pass">Heslo</label>
        <input class="form-control" type="password" name="pass" value="<?= $this->pass ?>"/>
        <small class="form-text text-muted">
          Pouze písmena bez diakritiky, číslice a podtržítka, 6 - 32 znaků
        </small>
      </div>
    </div>

    <div class="form-row">
      <div class="form-group col-12 col-md-6">
        <label for="jmeno">Jméno</label>
        <?= $this->text('jmeno', $this->jmeno) ?>
      </div>
      <div class="form-group col-12 col-md-6">
        <label for="prijmeni">Příjmení</label>
        <?= $this->text('prijmeni', $this->prijmeni) ?>
      </div>
    </div>

    <div class="form-group">
      <?= $this->radio('pohlavi', 'm')->set($this->pohlavi == 'm')->label('Muž')->inline() ?>
      <?= $this->radio('pohlavi', 'f')->set($this->pohlavi == 'f')->label('Žena')->inline() ?>
    </div>

    <div class="form-group">
      <label for="narozeni">Datum narození</label>
      <?= $this->date('narozeni', $this->narozeni) ?>
    </div>

    <div class="form-row">
      <div class="form-group col-12 col-md-6">
        <label for="email">E-mail</label>
        <?= $this->text('email', $this->email) ?>
      </div>
      <div class="form-group col-12 col-md-6">
        <label for="telefon">Telefon</label>
        <?= $this->text('telefon', $this->telefon) ?>
      </div>
    </div>

    <div class="form-group">
      <label for="skupina">Skupina</label>
      <?php
      $s = $this->select('skupina')->set($this->skupina ?: '10');
      foreach ($this->skupiny as $item) {
          $s->option(
              $item['id'],
              $this->colorbox($item['color'], $item['popis']) . '&nbsp;' . $item['popis']
          );
      }
      echo $s;
      ?>
    </div>

    <div class="form-group">
      <label for="poznamky">Vztah ke klubu</label>
      <div class="form-inline">
        <?= $this->radio('poznamky', 'dancer')
               ->set($this->poznamky == 'dancer')
               ->label('Tanečník/tanečníce') ?>
      </div>
      <div class="form-inline">
        <?php $i = $this->text('dancer-name', $this->dancerName)
                       ->placeholder('jméno tanečníka')
                       ->cls('form-control ml-2') ?>
        <?= $this->radio('poznamky', 'parent')
               ->set($this->poznamky == 'parent')
               ->label('Rodič tanečníka: ' . $i) ?>
      </div>
      <div class="form-inline">
        <?php $i = $this->text('other', $this->other)
                       ->placeholder('vztah ke klubu')
                       ->cls('form-control ml-2') ?>
        <?= $this->radio('poznamky', 'other')
               ->set($this->poznamky == 'other')
               ->label('Jiný ' . $i) ?>
      </div>
    </div>

    <?= $this->submit('Registrovat se')->data('action', 'register') ?>
  </form>
</div>
