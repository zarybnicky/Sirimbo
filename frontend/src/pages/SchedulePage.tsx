import * as React from 'react';
import format from 'date-fns/format';
import { Card, CardContent, Container, Grid, Typography } from '@material-ui/core';
import { useTypedQuery } from '../zeus/apollo';
import { useAuth } from '../data/use-auth';
import { Schedule, ScheduleItem, ScheduleRangeQuery } from '../data/use-schedule';
import { Reservation, ReservationItem, ReservationRangeQuery } from '../data/use-reservation';
import { PermissionLevel, usePermissions } from '../data/use-permissions';
import { formatDateRange } from '../custom-elements/date';

export const SchedulePage = ({ }) => {
  const { user, couple } = useAuth();
  const perms = usePermissions();
  const { data: schedules } = useTypedQuery(ScheduleRangeQuery, {
    variables: { startDate: '2022-02-01', endDate: '2022-04-01' },
  });
  const { data: reservations } = useTypedQuery(ReservationRangeQuery, {
    variables: { startDate: '2022-02-01', endDate: '2022-04-01' },
  });

  const canEditSchedule = (trainerId: number) => (
    (perms.peRozpis >= PermissionLevel.P_OWNED && user?.uId == trainerId) ||
    perms.peRozpis >= PermissionLevel.P_ADMIN
  );
  const canSignUp = (lesson: ScheduleItem, item: Schedule) => (
    perms.peRozpis >= PermissionLevel.P_MEMBER &&
    lesson.riPartner == 0 && !item.rLock && !lesson.riLock
  );
  const canSignOut = (lesson: ScheduleItem, item: Schedule) => (
    lesson.riPartner != 0 && !item.rLock && !lesson.riLock && (
      (perms.peRozpis >= PermissionLevel.P_MEMBER && couple?.pId == lesson.riPartner) ||
      (perms.peRozpis >= PermissionLevel.P_OWNED && user?.uId == item.rTrener) ||
      perms.peRozpis >= PermissionLevel.P_ADMIN
    )
  );
  const canEditReservation = (trainerId: number) => (
    (perms.peNabidka >= PermissionLevel.P_OWNED && user?.uId == trainerId) ||
    perms.peNabidka >= PermissionLevel.P_ADMIN
  );
  const canReserve = (item: Reservation) => (
    perms.peNabidka >= PermissionLevel.P_MEMBER && !item.nLock &&
    item.nPocetHod > item.nabidkaItemsByNiIdRodic.nodes.reduce((n, x) => n + x.niPocetHod, 0)
  );
  const canCancel = (lesson: ReservationItem, item: Reservation) => (
    lesson.niPartner != 0 && !item.nLock && !lesson.niLock && (
      (perms.peNabidka >= PermissionLevel.P_MEMBER && couple?.pId == lesson.niPartner) ||
      (perms.peNabidka >= PermissionLevel.P_OWNED && user?.uId == item.nTrener) ||
      perms.peNabidka >= PermissionLevel.P_ADMIN
    )
  );

  const scheduleList = (schedules?.schedulesForRange?.nodes || []).map((item, i) => (
    <Grid item key={i}>
      <Card>
        <CardContent>
          <div className="h5 mb-0">
            {item.userByRTrener?.uJmeno} {item.userByRTrener?.uPrijmeni}
            {canEditSchedule(item.userByRTrener?.uId) && (
              <div className="btn-group">
                <button type="button" className="btn btn-xs pt-0" data-toggle="dropdown">
                  <img alt="Upravit" width="16" src="/style/icon-gear.png" />
                </button>
                <div className="dropdown-menu dropdown-menu-right">
                  <a className="dropdown-item" href={`/admin/rozpis/edit/${item.rId}`}>Upravit</a>
                  <a className="dropdown-item" href={`/admin/rozpis/detail/${item.rId}`}>Upravit lekce</a>
                </div>
              </div>
            )}
          </div>
          <div className="date">{format(new Date(item.rDatum), 'd. M. y')}</div>
          <div>{item.rKde}</div>
          <hr />
          {(item.rozpisItemsByRiIdRodic.nodes || []).map((lesson, i) => (
            <Grid container key={i} spacing={2}>
              <Grid item>{lesson.riOd.slice(0, 5)}-{lesson.riDo.slice(0, 5)}</Grid>
              <Grid item style={{ flexGrow: 1 }}>
                {canSignUp(lesson, item) ? (
                  <button name="action" value="signup" className="py-0 btn btn-outline-primary btn-sm">+</button>
                ) : (
                  <>{lesson.paryByRiPartner?.userByPIdPartner?.uJmeno} {lesson.paryByRiPartner?.userByPIdPartner?.uPrijmeni}</>
                )}
              </Grid>
              {canSignOut(lesson, item) && (
                <Grid item>
                  <button name="action" value="signout" className="py-0 btn btn-outline-primary btn-sm">&times;</button>
                </Grid>
              )}
            </Grid>
          ))}
        </CardContent>
      </Card>
    </Grid>
  ));

  const reservationList = (reservations?.reservationsForRange?.nodes || []).map((item, i) => (
    <Grid item key={i}>
      <Card>
        <CardContent>
          <div className="h5 mb-0">
            {item.userByNTrener?.uJmeno} {item.userByNTrener?.uPrijmeni}
            {canEditReservation(item.nTrener) && (
              <div className="btn-group">
                <button type="button" className="btn btn-xs pt-0" data-toggle="dropdown">
                  <img alt="Upravit" width="16" src="/style/icon-gear.png" />
                </button>
                <div className="dropdown-menu dropdown-menu-right">
                  <a className="dropdown-item" href={`/admin/nabidka/edit/${item.nId}`}>Upravit</a>
                  <a className="dropdown-item" href={`/admin/nabidka/detail/${item.nId}`}>Upravit rezervace</a>
                </div>
              </div>
            )}
          </div>
          <div className="date">{formatDateRange(item.nOd, item.nDo)}</div>

          {item.nMaxPocetHod > 0 && <>
            <span className="little">Maximálně hodin/pár:</span>
            <span className="nadpis">{item.nMaxPocetHod}</span>
          </>}
          <div>
            <span className="little">Volných hodin: </span>
            <span className="nadpis">
              {item.nPocetHod - item.nabidkaItemsByNiIdRodic.nodes.reduce((n, x) => n + x.niPocetHod, 0)} z {item.nPocetHod} nabízených
            </span>
          </div>
          <hr />
          {item.nabidkaItemsByNiIdRodic.nodes.map((lesson, i) => (
            <div className="row mx-auto mb-1 no-gutters" key={i}>
              <div className="col-auto flex-grow-1 pr-1">
                {lesson.niPocetHod}x
                {lesson.paryByNiPartner?.userByPIdPartner?.uJmeno}
                {lesson.paryByNiPartner?.userByPIdPartner?.uPrijmeni}
              </div>
              {canCancel(lesson, item) && (
                <div className="col-2">
                  <button name="p_id" value="{lesson.p_id}"
                    className="pl-2 py-0 btn btn-outline-primary btn-sm">&times;</button>
                </div>
              )}
            </div>
          ))}
          {canReserve(item) && (
            <div className="form-inline text-center" style={{ padding: '10px 0 5px' }}>
              <input className="w-auto form-control" type="text" placeholder="Počet hodin" name="hodiny" />
              <button className="btn btn-primary">Rezervovat</button>
            </div>
          )}
        </CardContent>
      </Card>
    </Grid>
  ));

  return <Container maxWidth="lg" style={{ paddingTop: '2rem' }}>
    <Typography align="right" variant="h4" component="h2">Tento týden</Typography>
    <Grid container spacing={2}>
      {scheduleList}
      {reservationList}
    </Grid>
  </Container >;
}
