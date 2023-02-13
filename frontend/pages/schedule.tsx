import * as React from 'react';
import { usePermissions } from 'lib/data/use-permissions';
import { MyReservationFragment, useReservationRangeQuery } from 'lib/graphql/Reservation';
import { ScheduleFragment, useScheduleRangeQuery } from 'lib/graphql/Schedule';
import { Dropdown } from 'components/Dropdown';
import { LessonButton } from 'components/LessonButton';
import { Card } from 'components/Card';
import { MoreVertical } from 'react-feather';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { formatShortDateRange, formatWeekDay } from 'lib/format-date';
import { formatCoupleName } from 'lib/format-name';
import startOfWeek from 'date-fns/startOfWeek';
import endOfWeek from 'date-fns/endOfWeek';
import endOfYear from 'date-fns/endOfYear';
import classNames from 'classnames';
import { ReservationButton } from 'components/ReservationButton';
import { useAuth } from 'lib/data/use-auth';

export default function SchedulePage() {
  const [startDate, setStartDate] = React.useState(() => startOfWeek(new Date(), { weekStartsOn: 1 }));

  const { data: schedules } = useScheduleRangeQuery({
    startDate: startDate.toISOString().substring(0, 10),
    endDate: endOfWeek(startDate, { weekStartsOn: 1 }).toISOString().substring(0, 10),
  });
  const { data: reservations } = useReservationRangeQuery({
    startDate: startDate.toISOString().substring(0, 10),
    endDate: endOfYear(startDate).toISOString().substring(0, 10),
  });

  const scheduleByDay = React.useMemo(() => {
    const obj: { [date: string]: ScheduleFragment[] } = {};
    schedules?.schedulesForRange?.nodes?.forEach(item => {
      const arr = obj[item.rDatum] || [];
      arr.push(item);
      obj[item.rDatum] = arr;
    });
    return obj;
  }, [schedules])

  return <div className="col-full mt-12 mb-8 mx-4">
    <h4 className="text-2xl ml-3 tracking-wide">Tréninky</h4>

    <h4 className="text-2xl ml-3 tracking-wide">Nabídky nadcházejících tréninků</h4>
    <div className="flex flex-wrap gap-4">
      {reservations?.reservationsForRange?.nodes.map((item, i) => (
        <ReservationItem key={i} item={item} />
      ))}
    </div>

    {Object.entries(scheduleByDay).map(([date, items], i) => <React.Fragment key={i}>
      <div className="text-xl font-bold text-stone-700 mt-6 ml-3 mb-4">
        {formatWeekDay(new Date(date))}
      </div>

      <div className="flex flex-wrap gap-4">
        {items.map((item, i) => <ScheduleItem key={i} item={item} />)}
      </div>
    </React.Fragment>)}
  </div>;
}

const ReservationItem = ({ item }: { item: MyReservationFragment; }) => {
  const { couple } = useAuth();
  const perms = usePermissions();

  return (
    <div className="group relative min-w-[200px]">
      {perms.canEditReservation(item) && (
        <div className="absolute right-2 top-2">
          <Dropdown align="end"
            button={<MoreVertical className="text-stone-500 w-6 invisible ui-open:visible group-hover:visible" />}
            options={[
              { title: "Upravit", href: `/admin/nabidka/${item.id}` },
              { title: "Upravit rezervace", href: `/admin/nabidka/detail/${item.id}` },
            ]}
          />
        </div>
      )}

      <div className="ml-3 mb-0.5">
        <div className="font-bold">{formatShortDateRange(new Date(item.nOd), new Date(item.nDo))}</div>
        <div className="text-xl">{item.userByNTrener?.fullName}</div>

        {item.nMaxPocetHod > 0 && (
          <div>
            <span className="text-stone-500">Maximálně hodin/pár:</span>
            <span className="text-lg">{item.nMaxPocetHod}</span>
          </div>
        )}
        <div>
          <span className="text-stone-500">Volných hodin: </span>
          <span className="text-lg">{item.freeLessons} z {item.nPocetHod} nabízených</span>
        </div>
      </div>

      <Card className="grid mx-auto w-72 rounded-lg border-stone-200 border">
        {item.nabidkaItemsByNiIdRodic.nodes.filter(x => x.niPartner !== couple?.id).map((lesson, i) => (
          <div key={i} className={classNames(
            "group flex gap-3 p-2.5 rounded-lg",
            "leading-4 text-sm tabular-nums",
          )}>
            <div>{formatCoupleName(lesson.paryByNiPartner)}</div>
            <div className="grow text-right">{lesson.niPocetHod}x</div>
          </div>
        ))}
        {(perms.canMakeReservation(item) || item.myLessons) && (
          <ReservationButton item={item} />
        )}
      </Card>
    </div>
  );
};

const ScheduleItem = ({ item }: { item: ScheduleFragment; }) => {
  const perms = usePermissions();

  return (
    <div className="group relative min-w-[200px]">
      <div className="ml-3 mb-0.5">
        {perms.canEditSchedule(item) && (
          <div className="absolute right-2 top-2">
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
        <div className="text-xl">{item.userByRTrener?.fullName}</div>
      </div>

      <Card className="grid mx-auto w-72 rounded-lg border-stone-200 border">
        {item.rozpisItemsByRiIdRodic.nodes?.map((lesson, i) => (
          <LessonButton key={i} schedule={item} lesson={lesson} />
        ))}
      </Card>
    </div>
  );
};

export const getServerSideProps = withServerPermissions(
  PermissionKey.peRozpis, PermissionLevel.P_MEMBER,
);
