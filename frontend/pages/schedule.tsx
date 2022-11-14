import * as React from 'react';
import format from 'date-fns/format';
import { Card } from 'components/Card';
import { usePermissions } from 'lib/data/use-permissions';
import { DateRange } from 'components/DateRange';
import { useReservationRangeQuery, useScheduleRangeQuery } from 'lib/graphql';
import parse from 'date-fns/parse';
import { Dropdown } from 'components/Dropdown';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { Button } from 'components/Button';

export default function SchedulePage() {
  useRequireUserLoggedIn();
  const perms = usePermissions();
  const [startDate] = React.useState('2022-02-01');
  const [endDate] = React.useState('2022-03-01');
  const now = new Date();

  const { data: schedules } = useScheduleRangeQuery({ startDate, endDate });
  const { data: reservations } = useReservationRangeQuery({ startDate, endDate });

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

  const scheduleList = schedules?.schedulesForRange?.nodes?.map((item, i) => (
    <Card key={i}>
      <div className="h5 mb-0">
        {item.userByRTrener?.uJmeno} {item.userByRTrener?.uPrijmeni}
        {item.userByRTrener && perms.canEditSchedule(item) && <Dropdown
          button={<img alt="Upravit" width="16" src="/style/icon-gear.png" />}
          options={[
            { title: "Upravit", href: `/admin/rozpis/edit/${item.rId}` },
            { title: "Upravit rezervace", href: `/admin/rozpis/detail/${item.rId}` },
          ]}
        />}
      </div>
      <div className="text-slate">{format(new Date(item.rDatum), 'd. M. y')}</div>
      <div>{item.rKde}</div>
      <hr />

      <div className="grid grid-cols-[1fr_2fr]">
        {item.rozpisItemsByRiIdRodic.nodes?.map((lesson, i) => <React.Fragment key={i}>
          <div>
            {format(parse(lesson.riOd, "HH:mm:ss", now), 'HH:mm')}{'-'}
            {format(parse(lesson.riDo, "HH:mm:ss", now), 'HH:mm')}
          </div>
          <div>
            {perms.canSignUp(item, lesson) ? (
              <button name="action" value="signup" className="button button-icon button-red button-sm py-0">+</button>
            ) : <>
              {lesson.paryByRiPartner?.userByPIdPartner?.uJmeno}{' '}
              {lesson.paryByRiPartner?.userByPIdPartner?.uPrijmeni}
              {perms.canSignOut(item, lesson) && (
                <button name="action" value="signout" className="button button-icon button-sm button-red py-0">&times;</button>
              )}
            </>}
          </div>
        </React.Fragment>)}
      </div>
    </Card>
  ));

  const reservationList = reservations?.reservationsForRange?.nodes?.map((item, i) => (
    <Card key={i}>
      <div className="h5 mb-0">
        {item.userByNTrener?.uJmeno} {item.userByNTrener?.uPrijmeni}
        {perms.canEditReservation(item) && <Dropdown
          button={<img alt="Upravit" width="16" src="/style/icon-gear.png" />}
          options={[
            { title: "Upravit", href: `/admin/nabidka/edit/${item.nId}` },
            { title: "Upravit rezervace", href: `/admin/nabidka/detail/${item.nId}` },
          ]}
        />}
      </div>
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
          {lesson.niPocetHod}x{' '}
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
    </Card>
  ));

  return <div className="container mx-auto max-w-5xl mt-12 mb-8">
    <h4 className="text-right">Tento týden</h4>
    <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-4">
      {scheduleList}
      {reservationList}
    </div>
  </div>;
}
