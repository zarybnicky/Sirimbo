<?php
require_once 'files/Controller/Admin/Platby.php';
class Controller_Admin_Platby_Discarded extends Controller_Admin_Platby
{
    public function __construct()
    {
        Permissions::checkError('platby', P_OWNED);
    }

    public function view($request)
    {
        $data = DBPlatbyRaw::getDiscarded();
        if (count($data) == 0) {
            $this->redirect(
                '/admin/platby',
                'V databázi nejsou žádné vyřazené platby.'
            );
        }
        if ($request->get('list')) {
            $this->_getTable($request, $data, $result, $columns, $header);
            $this->render('files/View/Admin/Platby/DiscardedTable.inc', [
                'header' => 'Správa plateb',
                'subheader' => 'Vyřazené platby (' . $this->header . ')',
                'data' => $result,
                'columns' => $columns,
                'header' => $header,
                'uri' => $request->getLiteralURI()
            ]);
        } else {
            $this->_getList($data, $groupAmount, $groupDate);
            $this->render('files/View/Admin/Platby/DiscardedList.inc', [
                'header' => 'Správa plateb',
                'subheader' => 'Vyřazené platby',
                'groupByDate' => $groupDate,
                'groupByAmount' => $groupAmount,
                'uri' => $request->getLiteralURI()
            ]);
        }
    }

    public function remove($request)
    {
        $id = $request->getId();
        if (!$id && !DBPlatbyRaw::getSingle($id)) {
            $this->redirect(
                $request->getReferer(),
                'Platba se zadaným ID neexistuje.'
            );
        }

        DBPlatbyRaw::delete($id);
        $this->redirect(
            $request->getReferer(),
            'Platba byla odstraněna.'
        );
    }

    private function _getTable($request, $data, &$result, &$columns, &$header)
    {
        if ($request->get('list') == 'date') {
            $header =
                ($request->get('year') == 'none'
                ? 'nezařazené podle data'
                : ($request->get('month')
                  ? ($request->get('year') . '/' . $request->get('month'))
                  : $request->get('year')));
        } elseif ($request->get('list') == 'amount') {
            $header =
                ($request->get('amount') == 'none'
                ? 'nezařazené podle částky'
                : ($request->get('amount') . ' Kč'));
        } else {
            $header = 'všechny';
        }

        $result = [];
        $columnsTemp = [];
        foreach ($data as $rawData) {
            $row = unserialize($rawData['pr_raw']);
            if (!$this->checkHeaders(array_flip($row), $specific, $variable, $date, $amount)) {
                $this->recognizeHeaders($row, $specific, $variable, $date, $amount);
            }

            if ($request->get('list') == 'date') {
                if (isset($row[$date]) && $row[$date]) {
                    if ($request->get('year') == 'none') {
                        continue;
                    }
                    $currentDate = new Date($row[$date]);
                    if ($currentDate->getYear() != $request->get('year')
                        || ($request->get('month') && $currentDate->getMonth() != $request->get('month'))
                    ) {
                        continue;
                    }
                } elseif ($request->get('year') !== 'none') {
                    continue;
                }
            } elseif (
                $request->get('list') == 'amount'
                && ((!isset($row[$amount]) ^ $request->get('amount') == 'none')
                || $request->get('amount') != (int) $row[$amount])
            ) {
                continue;
            }
            foreach ($row as $key => $value) {
                if ($value) {
                    $columnsTemp[$key] = true;
                } elseif (!isset($columnsTemp[$key])) {
                    $columnsTemp[$key] = false;
                }
            }
            $row['edit'] = new Tag(
                'div',
                ['style' => 'width:51px'],
                $this->editLink('/admin/platby/manual/' . $rawData['pr_id']),
                $this->removeLink('/admin/platby/discarded/remove/' . $rawData['pr_id'])
            );
            $result[] = $row;
        }
        if (empty($columnsTemp)) {
            return;
        } else {
            foreach ($result as &$row) {
                foreach ($columnsTemp as $key => $value) {
                    if (!isset($row[$key])) {
                        $row[$key] = '';
                    }
                }
            }
        }

        $columns = [['edit', 'Zařadit']];
        foreach ($columnsTemp as $key => $value) {
            if (!$value) {
                continue;
            }
            $columns[] = [$key, $key];
        }
    }

    private function _getList($data, &$groupAmount, &$groupDate)
    {
        $groupDate = [];
        $groupAmount = [];
        foreach ($data as $row) {
            $row = unserialize($row['pr_raw']);
            if (!$this->checkHeaders(array_flip($row), $specific, $variable, $date, $amount)) {
                $this->recognizeHeaders($row, $specific, $variable, $date, $amount);
            }

            if (isset($row[$date]) && $row[$date]) {
                $currentDate = new Date($row[$date]);
                if (!isset($groupDate[$currentDate->getYear()])) {
                    $groupDate[$currentDate->getYear()] = ['name' => $currentDate->getYear()];
                }

                if (!isset($groupDate[$currentDate->getYear()]['months'][$currentDate->getMonth()])) {
                    $groupDate[$currentDate->getYear()]['months'][$currentDate->getMonth()] =
                        $currentDate->getYear() . '/' . $currentDate->getMonth();
                }
            } elseif (!isset($groupDate['none'])) {
                $groupDate['none'] = ['name' => 'Nerozpoznáno'];
            }

            if (isset($row[$amount])) {
                if (!isset($groupAmount[$row[$amount]])) {
                    $groupAmount[(int) $row[$amount]] = (int) $row[$amount] . ' Kč';
                }
            } elseif (!isset($groupAmount['none'])) {
                $groupAmount['none'] = 'Nerozpoznáno';
            }
        }
        krsort($groupAmount);
        krsort($groupDate);
        foreach ($groupDate as $year) {
            krsort($year);
        }
    }
}
