<div class="container">
<h1>Rozpis tréninků</h1>
<?php echo $this->notice($this->redirect()->getMessage()); ?>

<style type="text/css">
.unit {
  background:inherit;
  border:none;
  vertical-align:top;
  border-bottom:1px dotted #999;
  padding:10px 5px;
}
</style>

<table style="width:100%;">

<?php foreach (array_chunk($this->data, 2) as $chunk) : ?>
    <tr>
    <?php foreach ($chunk as $item) : ?>
      <td class="unit">
      <?php echo $this->render('files/View/Member/Rozpis/Header.inc', $item); ?>
        <table style="margin:0 auto;">
          <tr>
            <td>Od</td>
            <td>Do</td>
            <td>Tanečník</td>
          </tr>
          <?php foreach ($item['items'] as $row): ?>
            <form action="" method="post">
              <tr>
                <td><?php echo $row['timeFrom']; ?></td>
                <td><?php echo $row['timeTo']; ?></td>
                <td>
                  <?php echo $row['fullName']; ?>
                  <input type="hidden" name="ri_id" value="<?php echo $row['id'] ?>" />
                </td>
                <?php if ($row['canReserve']): ?>
                  <td>
                    <button type="submit" name="action" value="signup">
                      Rezervovat
                    </button>
                  </td>
                <?php endif; ?>
                <?php if ($row['canCancel']): ?>
                  <td>
                    <button type="submit" name="action" value="signout">
                      Zrušit
                    </button>
                  </td>
                <?php endif; ?>
              </tr>
            </form>
          <?php endforeach; ?>
        </table>
      </td>
    <?php endforeach; ?>
    </tr>
<?php endforeach; ?>
</table>
</div>