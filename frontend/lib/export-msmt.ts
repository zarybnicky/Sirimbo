export const exportMsmtCsv = () => null;

//     public static function getMsmtCsv()
//     {
//         \Permissions::checkError('users', P_OWNED);

//         $out = implode(';', [
//             'JMENO',
//             'DALSI_JMENA',
//             'PRIJMENI',
//             'DATUM_NAROZENI',

//             'NAZEV_OBCE',
//             'NAZEV_CASTI_OBCE',
//             'NAZEV_ULICE',
//             'CISLO_POPISNE',
//             'CISLO_ORIENTACNI',
//             'PSC',

//             'STRECHA',
//             'SVAZ',
//             'KLUB',
//             'ODDIL',

//             'DRUH_SPORTU',
//             'SPORTOVEC',
//             'TRENER',
//             'CLENSTVI_OD',
//             'CLENSTVI_DO',
//             'OBCANSTVI',
//             'EXT_ID'
//         ]);

//         $oldest = \DBPlatby::getOldestPayment();
//         $newest = \DBPlatby::getNewestPayment();
//         foreach (\DBUser::getUsers() as $u) {
//             if ($u['u_ban'] || !$u['u_confirmed'] || $u['u_system']) {
//                 continue;
//             }
//             // skupina - ne Host/VIP
//             if (in_array($u['u_skupina'], ['9', '10', '13'])) {
//                 continue;
//             }
//             // od 1.9.2019
//             if (isset($newest[$u['u_id']]) && new \DateTime($newest[$u['u_id']]) < new \DateTime('2019-09-01')) {
//                 continue;
//             }

//             $out .= '
// ' . implode(';', [
//                 $u['u_jmeno'],
//                 '',
//                 $u['u_prijmeni'],
//                 implode('.', array_reverse(explode('-', $u['u_narozeni']))),
//                 $u['u_city'],
//                 $u['u_district'],
//                 $u['u_street'],
//                 $u['u_conscription_number'],
//                 $u['u_orientation_number'],
//                 str_replace(' ', '', $u['u_postal_code']),
//                 '',
//                 '',
//                 '',
//                 '',
//                 '66',
//                 $u['u_dancer'] ? '1' : '0',
//                 $u['u_teacher'] ? '1' : '0',
//                 isset($oldest[$u['u_id']])
//                 ? implode('.', array_reverse(explode('-', $oldest[$u['u_id']])))
//                 : '',
//                 isset($newest[$u['u_id']])
//                 ? implode('.', array_reverse(explode('-', $newest[$u['u_id']])))
//                 : '',
//                 $u['u_nationality'],
//                 ''
//             ]);
//         }

//         header('Pragma: no-cache');
//         header('Content-Type: text/csv');
//         header('Content-Disposition: inline; filename="olymp-msmt-export.csv');
//         echo chr(239) . chr(187) . chr(191) . $out;
//     }


// 1806875329/0800
