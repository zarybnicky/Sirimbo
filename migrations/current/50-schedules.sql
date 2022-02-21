create or replace function public.schedules_for_range(start_date date, end_date date) returns setof rozpis as $$
  select * from rozpis
  where r_visible=true
  and r_datum >= start_date and r_datum <= end_date
  order by r_datum asc;
$$ language sql stable;

create or replace function public.reservations_for_range(start_date date, end_date date) returns setof nabidka as $$
  select * from nabidka
  where n_visible=true
  and n_od <= start_date and n_do >= end_date
  order by n_od asc;
$$ language sql stable;


-- CREATE OR REPLACE FUNCTION public.schedule_lesson() RETURNS BOOLEAN AS $$
-- DECLARE
-- BEGIN
--                 if (!\Session::getZaplacenoPar()) {
--                     \Message::warning('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
--                 } elseif ($lesson['ri_partner']) {
--                     \Message::warning('Lekce už je obsazená');
--                 } else {
--                     \DBRozpis::reserveLesson($_POST['ri_id'], $par['p_id']);
--                 }
-- END;
-- $$ LANGUAGE plpgsql SECURITY DEFINER;
-- select plpgsql_check_function('app_private.signup_schedule');

-- CREATE OR REPLACE FUNCTION public.unschedule_lesson() RETURNS BOOLEAN AS $$
-- DECLARE
-- BEGIN
--     if ($par['p_id'] != $lesson['ri_partner'] && !\Permissions::check('rozpis', P_OWNED, $data['r_trener'])) {
--         \Message::warning('Nedostatečná oprávnění!');
--     } else {
--         \DBRozpis::cancelLesson($_POST['ri_id']);
--     }
-- END;
-- $$ LANGUAGE plpgsql SECURITY DEFINER;
-- select plpgsql_check_function('app_private.signup_schedule');


-- CREATE OR REPLACE FUNCTION public.reserve_lesson() RETURNS BOOLEAN AS $$
-- DECLARE
-- BEGIN
--             $form = new \Form();
--             $form->checkBool(!$data['n_lock'], 'Tato nabídka je uzamčená');
--             if ($_POST['hodiny'] ?? null) {
--                 $form->checkNumeric($_POST['hodiny'], 'Špatný počet hodin');
--             }
--             if ($_POST['hodiny'] ?? null) {
--                 if (!\Session::getZaplacenoPar()) {
--                     \Message::danger('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
--                 } elseif ($data['n_max_pocet_hod'] > 0 &&
--                           (\DBNabidka::getNabidkaLessons($nId, $par['p_id']) + $_POST['hodiny']) > $data['n_max_pocet_hod']
--                 ) {
--                     \Message::danger('Maximální počet hodin na pár je ' . $data['n_max_pocet_hod'] . '!');
--                 } elseif (($data['n_pocet_hod'] - \DBNabidka::getReservationLessons($nId)) < $_POST['hodiny']) {
--                     \Message::danger('Tolik volných hodin tu není');
--                 } else {
--                     \DBNabidka::addNabidkaItemLessons($par['p_id'], $nId, $_POST['hodiny']);
--                 }
-- END;
-- $$ LANGUAGE plpgsql SECURITY DEFINER;
-- select plpgsql_check_function('app_private.signup_schedule');

-- CREATE OR REPLACE FUNCTION public.cancel_reservation(ni_id int) RETURNS BOOLEAN AS $$
-- DECLARE

-- BEGIN
--             $form->checkBool(!$data['n_lock'], 'Tato nabídka je uzamčená');
--              if (!\DBNabidka::getNabidkaLessons($nId, $_POST['p_id'])) {
--                  \Message::danger('Neplatný požadavek!');
--              } elseif ($_POST['p_id'] != $par['p_id'] &&
--                        !\Permissions::check('nabidka', P_OWNED, $data['n_trener'])
--              ) {
--                  \Message::danger('Nedostatečná oprávnění!');
--              } else {
--                  \DBNabidka::removeNabidkaItem($nId, $_POST['p_id']);
--              }
-- END;
-- $$ LANGUAGE plpgsql SECURITY DEFINER;
-- select plpgsql_check_function('app_private.signup_schedule');
