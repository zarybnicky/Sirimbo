<?php
echo $this->render('files/View/Admin/Platby/OverviewNavigation.inc');
notice($this->redirect()->getRedirectMessage());
header_main('Vyřazené platby (' . $this->header . ')');
echo $this->table()
	->data($this->data)
	->columns($this->columns);