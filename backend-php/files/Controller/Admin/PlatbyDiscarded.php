<?php
namespace Olymp\Controller\Admin;

class PlatbyDiscarded
{
    public static function view()
    {
        \Permissions::checkError('platby', P_OWNED);
        $data = \DBPlatbyRaw::getDiscarded();
        if (count($data) == 0) {
            \Message::info('V databázi nejsou žádné vyřazené platby.');
            \Redirect::to('/admin/platby');
        }
        if (isset($_GET['list'])) {
            self::_getTable($data, $result, $columns, $header);
            \Render::twig('Admin/PlatbyDiscardedTable.twig', [
                'data' => $result,
                'columns' => $columns,
                'header' => $header,
            ]);
        } else {
            self::_getList($data, $groupAmount, $groupDate);
            \Render::twig('Admin/PlatbyDiscardedList.twig', [
                'groupByDate' => $groupDate,
                'groupByAmount' => $groupAmount,
            ]);
        }
    }

    public static function remove($id)
    {
        \Permissions::checkError('platby', P_OWNED);
        if (!\DBPlatbyRaw::getSingle($id)) {
            \Message::info('Platba se zadaným ID neexistuje.');
            \Redirect::to($_SERVER['HTTP_REFERER'] ?? '/admin/platby/manual');
        }
        \Database::query("DELETE FROM platby_raw WHERE pr_id='?'", $id);
        \Message::success('Platba byla odstraněna.');
        \Redirect::to($_SERVER['HTTP_REFERER'] ?? '/admin/platby/manual');
    }

    private static function _getTable($data, &$result, &$columns, &$header)
    {
        if ($_GET['list'] == 'date') {
            $header =
                $_GET['year'] == 'none' ? 'nezařazené podle data'
                : ($_GET['month'] ? "{$_GET['year']}/{$_GET['month']}" : $_GET['year']);
        } elseif ($_GET['list'] == 'amount') {
            $header = $_GET['amount'] == 'none' ? 'nezařazené podle částky' : "{$_GET['amount']} Kč";
        } else {
            $header = 'všechny';
        }

        $result = [];
        $columnsTemp = [];
        foreach ($data as $rawData) {
            $row = unserialize(stream_get_contents($rawData['pr_raw']));
            if (!Platby::checkHeaders(array_flip($row), $specific, $variable, $date, $amount)) {
                Platby::recognizeHeaders($row, $specific, $variable, $date, $amount);
            }

            if ($_GET['list'] == 'date') {
                if (isset($row[$date]) && $row[$date]) {
                    if ($_GET['year'] == 'none') {
                        continue;
                    }
                    $currentDate = new \Date($row[$date]);
                    if ($currentDate->getYear() != $_GET['year']
                        || ($_GET['month'] && $currentDate->getMonth() != $_GET['month'])
                    ) {
                        continue;
                    }
                } elseif ($_GET['year'] !== 'none') {
                    continue;
                }
            } elseif ($_GET['list'] == 'amount'
                      && ((!isset($row[$amount]) ^ $_GET['amount'] == 'none')
                         || $_GET['amount'] != (int) $row[$amount])
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
            $row['id'] = $rawData['pr_id'];
            $result[] = $row;
        }
        foreach ($result as &$row) {
            foreach ($columnsTemp as $key => $value) {
                if (!isset($row[$key])) {
                    $row[$key] = '';
                }
            }
        }
        foreach (array_filter($columnsTemp) as $key => $value) {
            $columns[] = $key;
        }
    }

    private static function _getList($data, &$groupAmount, &$groupDate)
    {
        $groupDate = [];
        $groupAmount = [];
        foreach ($data as $row) {
            $row = unserialize(stream_get_contents($row['pr_raw']));
            if (!Platby::checkHeaders(array_flip($row), $specific, $variable, $date, $amount)) {
                Platby::recognizeHeaders($row, $specific, $variable, $date, $amount);
            }

            if (isset($row[$date]) && $row[$date]) {
                $currentDate = new \Date($row[$date]);
                if (!isset($groupDate[$currentDate->getYear()])) {
                    $groupDate[$currentDate->getYear()] = ['name' => $currentDate->getYear(), 'months' => []];
                }
                if (!isset($groupDate[$currentDate->getYear()]['months'][$currentDate->getMonth()])) {
                    $groupDate[$currentDate->getYear()]['months'][$currentDate->getMonth()] =
                        $currentDate->getYear() . '/' . $currentDate->getMonth();
                }
            } elseif (!isset($groupDate['none'])) {
                $groupDate['none'] = ['name' => 'Nerozpoznáno', 'months' => []];
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
