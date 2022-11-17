import * as React from 'react';
import { format } from 'date-fns';
import { usePermissions } from 'lib/data/use-permissions';
import { DateRange } from 'components/DateRange';
import { ReservationFragment, useReservationRangeQuery } from 'lib/graphql/Reservation';
import { ScheduleFragment, ScheduleItemFragment, useScheduleRangeQuery } from 'lib/graphql/Schedule';
import { Dropdown } from 'components/Dropdown';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { Button } from 'components/Button';
import { cs } from 'date-fns/locale'

export default function SchedulePage() {
  useRequireUserLoggedIn();
  const perms = usePermissions();
  const [startDate] = React.useState('2022-02-01');
  const [endDate] = React.useState('2022-03-01');

  const { data: schedules } = useScheduleRangeQuery({ startDate, endDate });
  const { data: reservations } = useReservationRangeQuery({ startDate, endDate });

  const planList = React.useMemo(() => {
    const obj: { [date: string]: (ReservationFragment | ScheduleFragment)[] } = {};
    schedules?.schedulesForRange?.nodes?.forEach(item => {
      const arr = obj[item.rDatum] || [];
      arr.push(item);
      obj[item.rDatum] = arr;
    });
    reservations?.reservationsForRange?.nodes?.forEach(item => {
      const arr = obj[item.nOd] || [];
      arr.push(item);
      obj[item.nOd] = arr;
    });
    return obj;
  }, [schedules, reservations])

  const openLesson = (item: ScheduleFragment, lesson: ScheduleItemFragment) => { };

  // can reserve === has paid
  // $paid = \DBPlatby::hasPaidMemberFees(self::getUser()->getId);
  // $par = \DBPary::getLatestPartner($user->getId(), $user->getGender());
  // if ($par) {
  //     $paid = $paid && \DBPlatby::hasPaidMemberFees($par['u_id']);
  // }
  // return $paid;

  const bookLesson = React.useCallback((id: string) => {
    /* if (!\Session::getZaplacenoPar()) {
     *   \Message::warning('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
     * } elseif ($lesson['ri_partner']) {
     *   \Message::warning('Lekce už je obsazená');
     * } else {
         if ri_partner is NULL, then
     *   self::query("UPDATE rozpis_item SET ri_partner='?' WHERE ri_id='?'", $uid, $rid);
     * } */
  }, []);

  const cancelLesson = React.useCallback((id: string) => {
    /* if ($lesson['ri_partner'] === null) {
     * } elseif ($par['p_id'] != $lesson['ri_partner']
     *           && !\Permissions::check('rozpis', P_OWNED, $data['r_trener'])
     * ) {
     *   \Message::warning('Nedostatečná oprávnění!');
     * } else {
     *   self::query("UPDATE rozpis_item SET ri_partner=NULL WHERE ri_id='?'", $rid)
     * } */
  }, []);

  const reserveLessons = React.useCallback((id: string) => {
    // check lock
    // check lesson count
    /* if (!\Session::getZaplacenoPar()) {
     *   \Message::danger('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
     * } elseif ($data['n_max_pocet_hod'] > 0 &&
     *           (\DBNabidka::getNabidkaLessons($nId, $par['p_id']) + $_POST['hodiny']) > $data['n_max_pocet_hod']
     *             ) {
     *   \Message::danger('Maximální počet hodin na pár je ' . $data['n_max_pocet_hod'] . '!');
     * } elseif (($data['n_pocet_hod'] - \DBNabidka::getReservationLessons($nId)) < $_POST['hodiny']) {
     *   \Message::danger('Tolik volných hodin tu není');
     * } else {
     *   \DBNabidka::addNabidkaItemLessons($par['p_id'], $nId, $_POST['hodiny']);
     * } */
  }, []);

  const cancelLessonsReservation = React.useCallback((id: string) => {
    // check lock
    // check lesson count
    /* if (!\DBNabidka::getNabidkaLessons($nId, $_POST['p_id'])) {
     *   \Message::danger('Neplatný požadavek!');
     * } elseif ($_POST['p_id'] != $par['p_id'] &&
     *           !\Permissions::check('nabidka', P_OWNED, $data['n_trener'])
     * ) {
     *   \Message::danger('Nedostatečná oprávnění!');
     * } else {
     *   \DBNabidka::removeNabidkaItem($nId, $_POST['p_id']);
     * } */
  }, []);

  return <div className="container mx-auto max-w-5xl mt-12 mb-8">
    <h4 className="text-lg font-bold mb-2 text-right">Tento týden</h4>
    {Object.entries(planList).map(([date, items]) => <>
      <div className="text-xl font-bold text-slate-700 mt-6 mb-2">
        {format(new Date(date), 'EEEE d. M.', { locale: cs })}
      </div>
      <div className="flex gap-4">
        {items.map((item, i) => item.__typename === 'Rozpi' ? (
          <div key={i} className="min-w-[200px]">
            {perms.canEditSchedule(item) && <Dropdown align="center"
              button={<img className="w-4 absolute top-2 right-2" alt="Upravit" src="/style/icon-gear.png" />}
              options={[
                { title: "Upravit", href: `/admin/rozpis/edit/${item.rId}` },
                { title: "Upravit rezervace", href: `/admin/rozpis/detail/${item.rId}` },
              ]}
            />}

            <div>{item.rKde}</div>
            <div className="text-xl mb-2">
              {item.userByRTrener?.uJmeno} {item.userByRTrener?.uPrijmeni}
            </div>

            <div className="grid gap-1.5">
              {item.rozpisItemsByRiIdRodic.nodes?.map((lesson, i) => (
                <div key={i} onClick={() => openLesson(item, lesson)} className="cursor-pointer px-2 pt-1 rounded-md bg-slate-50">
                  <div className="text-stone-700 leading-4 text-sm tabular-nums">
                    {lesson.riOd.substring(0, 5)}&#8209;{lesson.riDo.substring(0, 5)}
                  </div>
                  {perms.canSignUp(item, lesson) ? (
                    <div>
                      VOLNÁ
                      <button name="action" value="signup" className="button button-icon button-red button-sm py-0">+</button>
                    </div>
                  ) : lesson.paryByRiPartner ? (
                    <div className="leading-6">
                      {['.',
                        ',', undefined].includes(lesson.paryByRiPartner?.userByPIdPartner?.uPrijmeni)
                        ? lesson.paryByRiPartner?.userByPIdPartner?.uJmeno : lesson.paryByRiPartner?.userByPIdPartner?.uPrijmeni}
                      {lesson.paryByRiPartner?.userByPIdPartnerka
                        ? ` - ${lesson.paryByRiPartner?.userByPIdPartnerka?.uPrijmeni}`
                        : ''}
                      {perms.canSignOut(item, lesson) && (
                        <button name="action" value="signout" className="button button-icon button-sm button-red py-0">&times;</button>
                      )}
                    </div>
                  ) : "-"}
                </div>
              ))}
            </div>
          </div>
        ) : item.__typename === 'Nabidka' ? (
          <div key={i}>
            {perms.canEditReservation(item) && <Dropdown align="center"
              button={<img className="w-4 absolute top-2 right-2" alt="Upravit" src="/style/icon-gear.png" />}
              options={[
                { title: "Upravit", href: `/admin/nabidka/edit/${item.nId}` },
                { title: "Upravit rezervace", href: `/admin/nabidka/detail/${item.nId}` },
              ]}
            />}
            <div className="h5 mb-0">{item.userByNTrener?.uJmeno} {item.userByNTrener?.uPrijmeni}</div>
            <div className="font-bold"><DateRange from={item.nOd} to={item.nDo} /></div>

            {item.nMaxPocetHod > 0 && <>
              <span className="text-slate-500">Maximálně hodin/pár:</span>
              <span className="text-lg">{item.nMaxPocetHod}</span>
            </>}
            <div>
              <span className="text-slate-500">Volných hodin: </span>
              <span className="text-lg">
                {item.nPocetHod - item.nabidkaItemsByNiIdRodic.nodes.reduce((n, x) => n + x.niPocetHod, 0)} z {item.nPocetHod} nabízených
              </span>
            </div>
            <hr />
            {item.nabidkaItemsByNiIdRodic.nodes.map((lesson, i) => (
              <div className="mx-auto mb-1 no-gutters" key={i}>
                {lesson.niPocetHod}{'x '}
                {lesson.paryByNiPartner?.userByPIdPartner?.uJmeno}{' '}
                {lesson.paryByNiPartner?.userByPIdPartner?.uPrijmeni}
                {perms.canCancelReservation(item, lesson) && (
                  <Button name="p_id" value="{lesson.p_id}" className="pl-2">&times;</Button>
                )}
              </div>
            ))}
            {perms.canMakeReservation(item) && (
              <div className="form-inline text-center" style={{ padding: '10px 0 5px' }}>
                <input className="w-auto form-control" type="text" placeholder="Počet hodin" name="hodiny" />
                <Button onClick={() => reserveLessons(item.nId)}>Rezervovat</Button>
              </div>
            )}
          </div>
        ) : null)}
      </div>
    </>)}
  </div>;
}
