<?php
namespace TKOlomouc\Controller\Admin\Akce;

use TKOlomouc\Controller\Admin\Akce;
use TKOlomouc\Utility\Permissions;
use TKOlomouc\Model\DBAkce;
use TKOlomouc\Model\DBDokumenty;
use TKOlomouc\Settings;

class Dokumenty extends Akce
{
    public function __construct()
    {
        Permissions::checkError('akce', P_OWNED);
    }

    public function view($id = null)
    {
        if (!$id || !($akce = DBAkce::getSingleAkce($id))) {
            $this->redirect('/admin/akce', 'Akce s takovým ID neexistuje');
        }
        $dokumentyCurrent = unserialize($akce['a_dokumenty']);

        if (!empty($_POST)) {
            if (post('remove') !== null) {
                $removeId = array_search(post('remove'), $dokumentyCurrent);
                unset($dokumentyCurrent[$removeId]);

                $dokumentyCurrent = array_values($dokumentyCurrent);
                $changed = true;
            }
            if (post('add-id') && DBDokumenty::getSingleDokument(post('add-id'))) {
                $dokumentyCurrent[] = post('add-id');
                post('add-id', 0);
                $changed = true;
            }
            if (isset($changed) && $changed) {
                DBAkce::editAkce(
                    $akce['a_id'], $akce['a_jmeno'], $akce['a_kde'],
                    $akce['a_info'], $akce['a_od'], $akce['a_do'],
                    $akce['a_kapacita'], serialize($dokumentyCurrent),
                    $akce['a_lock'], $akce['a_visible']
                );
                $akce = DBAkce::getSingleAkce($id);
            }
        }

        $dokumentyData = DBDokumenty::getMultipleById($dokumentyCurrent);
        foreach ($dokumentyData as &$item) {
            $newData = array(
                'id' => $item['d_id'],
                'name' => $item['d_name'],
                'type' => Settings::$documentTypes[$item['d_kategorie']]
            );
            $item = $newData;
        } unset($item);

        $dokumentyAll = DBDokumenty::getDokumenty();
        foreach ($dokumentyAll as &$item) {
            $newData = array(
                'id' => $item['d_id'],
                'name' => $item['d_name'],
                'type' => Settings::$documentTypes[$item['d_kategorie']]
            );
            $item = $newData;
        } unset($item);

        $items = DBAkce::getAkceItems($id);
        $newData = array(
            'id'    => $akce['a_id'],
            'jmeno' => $akce['a_jmeno'],
            'kde'   => $akce['a_kde'],
            'datum' => formatDate($akce['a_od'])
                . (($akce['a_od'] != $akce['a_do'])
                ? ' - ' . formatDate($akce['a_do']) : ''),
            'kapacita' => $akce['a_kapacita'],
            'volno'    => $akce['a_kapacita'] - count($items),
            'showForm' => Permissions::check('akce', P_MEMBER) && !$akce['a_lock'],
            'canEdit'  => Permissions::check('akce', P_OWNED),
            'info'     => nl2br($akce['a_info'])
        );
        $akce = $newData;

        $this->render(
            'src/application/View/Admin/Akce/Dokumenty.inc',
            array(
                'data' => $akce,
                'dokumentyCurrent' => $dokumentyCurrent,
                'dokumentyAll' => $dokumentyAll
            )
        );
    }
}
