import * as React from 'react';
import { format } from 'date-fns';
import { usePermissions } from 'lib/data/use-permissions';
import { DateRange } from 'components/DateRange';
import { ReservationFragment, useReservationRangeQuery } from 'lib/graphql/Reservation';
import { ScheduleFragment, useScheduleRangeQuery } from 'lib/graphql/Schedule';
import { Dropdown } from 'components/Dropdown';
import { Button } from 'components/Button';
import { cs } from 'date-fns/locale'
import { LessonButton } from 'components/LessonButton';
import { Card } from 'components/Card';
import { MoreVertical } from 'react-feather';
import { Layout } from 'components/layout/Layout';
import { capitalize } from 'lib/capitalize';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function SchedulePage() {
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

  const reserveLessons = React.useCallback((id: string) => {
    // check lock
    // check lesson count
    /* if (!\Session::getZaplacenoPar()) {
     *   \Message::danger('Buď vy nebo váš partner(ka) nemáte zaplacené členské příspěvky');
     * } elseif ($data['n_max_pocet_hod'] > 0 &&
     *           (\DBNabidka::getNabidkaLessons($id, $par['p_id']) + $_POST['hodiny']) > $data['n_max_pocet_hod']
     *             ) {
     *   \Message::danger('Maximální počet hodin na pár je ' . $data['n_max_pocet_hod'] . '!');
     * } elseif (($data['n_pocet_hod'] - \DBNabidka::getReservationLessons($id)) < $_POST['hodiny']) {
     *   \Message::danger('Tolik volných hodin tu není');
     * } else {
     *   \DBNabidka::addNabidkaItemLessons($par['p_id'], $id, $_POST['hodiny']);
     * } */
  }, []);

  const cancelLessonsReservation = React.useCallback((id: string) => {
    // check lock
    // check lesson count
    /* if (!\DBNabidka::getNabidkaLessons($id, $_POST['p_id'])) {
     *   \Message::danger('Neplatný požadavek!');
     * } elseif ($_POST['p_id'] != $par['p_id'] &&
     *           !\Permissions::check('nabidka', P_OWNED, $data['n_trener'])
     * ) {
     *   \Message::danger('Nedostatečná oprávnění!');
     * } else {
     *   \DBNabidka::removeNabidkaItem($id, $_POST['p_id']);
     * } */
  }, []);

  return <div className="col-popout mt-12 mb-8">
    {Object.entries(planList).map(([date, items]) => <>
      <div className="text-xl font-bold text-stone-700 mt-6 ml-3 mb-4">
        {capitalize(format(new Date(date), 'EEEE d. M.', { locale: cs }))}
      </div>
      <div className="flex flex-wrap gap-4">
        {items.map((item, i) => item.__typename === 'Rozpi' ? (
          <div key={i} className="group relative min-w-[200px]">
            <div className="ml-3 mb-0.5">
              {perms.canEditSchedule(item) && (
                <div className="absolute right-2 top-0">
                  <Dropdown align="end"
                    button={<MoreVertical className="text-stone-500 w-6 invisible ui-open:visible group-hover:visible" />}
                    options={[
                      { title: "Upravit", href: `/admin/rozpis/${item.id}` },
                      { title: "Upravit rezervace", href: `/admin/rozpis/detail/${item.id}` },
                    ]}
                  />
                </div>
              )}

              <div className="text-sm text-stone-500">{item.rKde}</div>
              <div className="text-xl">
                {item.userByRTrener?.uJmeno} {item.userByRTrener?.uPrijmeni}
              </div>
            </div>

            <Card className="grid mx-auto w-72 rounded-xl border-stone-200 border">
              {item.rozpisItemsByRiIdRodic.nodes?.map((lesson, i) => (
                <LessonButton key={i} schedule={item} lesson={lesson} />
              ))}
            </Card>
          </div>
        ) : item.__typename === 'Nabidka' ? (
          <div key={i}>
            {perms.canEditReservation(item) && <Dropdown align="center"
              button={<img className="w-4 absolute top-2 right-2" alt="Upravit" src="/style/icon-gear.png" />}
              options={[
                { title: "Upravit", href: `/admin/nabidka/${item.id}` },
                { title: "Upravit rezervace", href: `/admin/nabidka/detail/${item.id}` },
              ]}
            />}
            <div className="h5 mb-0">{item.userByNTrener?.uJmeno} {item.userByNTrener?.uPrijmeni}</div>
            <div className="font-bold"><DateRange from={item.nOd} to={item.nDo} /></div>

            {item.nMaxPocetHod > 0 && <>
              <span className="text-stone-500">Maximálně hodin/pár:</span>
              <span className="text-lg">{item.nMaxPocetHod}</span>
            </>}
            <div>
              <span className="text-stone-500">Volných hodin: </span>
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
                <Button onClick={() => reserveLessons(item.id)}>Rezervovat</Button>
              </div>
            )}
          </div>
        ) : null)}
      </div>
    </>)}
  </div>;
}

SchedulePage.getLayout = (page: React.ReactElement) => <Layout>{page}</Layout>;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peRozpis, PermissionLevel.P_MEMBER,
);
