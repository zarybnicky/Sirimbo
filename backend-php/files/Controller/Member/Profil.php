<?php
namespace Olymp\Controller\Member;

class Profil
{
    public static function get()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        $user = \Session::getUser();
        $s = \DBSkupiny::getSingle($user->getTrainingGroup());

        $history = \Database::queryArray(
            "SELECT * FROM platby_item INNER JOIN platby_category ON pi_id_category=pc_id
             WHERE pi_id_user='?'
             ORDER BY pi_date DESC",
            $user->getId(),
        );
        $paymentsPaid = array_flip(array_column($history, 'pc_id'));
        $paymentsWanted = [];
        $groups = \DBSkupiny::getSingleWithCategories($user->getTrainingGroup());
        foreach ($groups as $row) {
            if (!$row['pc_visible'] || isset($paymentsPaid[$row['pc_id']])) {
                continue;
            }
            $paymentsWanted[] = [
                'name' => $row['pc_name'],
                'type' => $row['pg_type'] ? 'Členské příspěvky' : 'Ostatní platby',
                'symbol' => $row['pc_symbol'],
                'amount' => $row['pc_amount'] * ($row['pc_use_base'] ? $row['pg_base'] : 1),
                'dueDate' => $row['pc_date_due'],
            ];
        }

        $row = \Database::querySingle(
            "SELECT COUNT(*) as count FROM platby_item
                INNER JOIN platby_category ON pc_id=pi_id_category
                INNER JOIN platby_category_group ON pcg_id_category=pc_id
                INNER JOIN platby_group ON pg_id=pcg_id_group
                INNER JOIN platby_group_skupina ON pgs_id_group=pg_id
                INNER JOIN skupiny ON pgs_id_skupina=s_id
                INNER JOIN users ON pi_id_user=u_id
            WHERE
                pg_type='1' AND
                u_id='?' AND
                u_skupina=s_id AND
                CURRENT_DATE >= pc_valid_from AND
                CURRENT_DATE <= pc_valid_to",
            $user->getId(),
        );
        $hasPaid = !!$row['count'];

        \Render::twig('Member/Profil.twig', [
            'user' => $user,
            'skupina' => [
                'name' => $s['s_name'],
                'color' => $s['s_color_rgb'],
            ],
            'hasPaid' => $hasPaid,
            'paymentHistory' => array_map(
                fn($row) => [
                    'id' => $row['pc_id'],
                    'name' => $row['pc_name'],
                    'symbol' => $row['pc_symbol'],
                    'amount' => $row['pi_amount'],
                    'paidOn' => $row['pi_date'],
                    'validFrom' => $row['pc_valid_from'],
                    'validUntil' => $row['pc_valid_to'],
                ],
                $history,
            ),
            'paymentsWanted' => $paymentsWanted,
        ]);
    }

    public static function heslo()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        \Render::twig('Member/ProfilNewPassword.twig');
    }

    public static function hesloPost()
    {
        \Permissions::checkError('nastenka', P_VIEW);
        $form = self::checkDataHeslo();
        if (!$form->isValid()) {
            \Message::warning($form->getMessages());
            \Render::twig('Member/ProfilNewPassword.twig');
            return;
        }
        \Database::query("select reset_password('?', '?')", \Session::getUser()->getId(), \User::crypt($_POST['newpass']));
        \Redirect::to('/member/profil');
    }

    private static function checkDataHeslo(): \Form
    {
        $data = \Database::querySingle("SELECT * FROM users WHERE LOWER(u_login)='?' AND u_pass='?'", \Session::getUser()->getLogin(), \User::crypt($_POST['oldpass']));
        $f = new \Form();
        $f->checkPassword($_POST['newpass'], 'Neplatný formát hesla');
        $f->checkBool($data, 'Staré heslo je špatně');
        $f->checkBool($_POST['newpass'] == $_POST['newpass_confirm'], 'Nová hesla se neshodují');
        return $f;
    }
}
