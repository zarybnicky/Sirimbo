import * as React from 'react';
import { useReservationRangeQuery } from 'lib/graphql/Reservation';
import { ScheduleFragment, useScheduleRangeQuery } from 'lib/graphql/Schedule';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { formatWeekDay } from 'lib/format-date';
import { ScheduleItem } from 'components/ScheduleItem';
import { ReservationItem } from 'components/ReservationItem';
import { getCurrentMonday, mondayToWeekRange, mondayToYearRange, WeekPicker } from 'components/WeekPicker';

export default function SchedulePage() {
  const [startDate, setStartDate] = React.useState(getCurrentMonday);

  const { data: schedules } = useScheduleRangeQuery(mondayToWeekRange(startDate));
  const { data: reservations } = useReservationRangeQuery(mondayToYearRange(startDate));

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
    <div className="flex items-start">
      <WeekPicker title="Tréninky" startDate={startDate} onChange={setStartDate} />
    </div>

    {(reservations?.reservationsForRange?.nodes.length ?? 0) > 0 && <>
      <div className="text-xl font-bold text-stone-700 mt-6 ml-3 mb-4">
        Nabídky tréninků
      </div>
      <div className="flex flex-wrap gap-4">
        {reservations?.reservationsForRange?.nodes.map((item, i) => <ReservationItem key={i} item={item} />)}
      </div>
    </>}

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

export const getServerSideProps = withServerPermissions(
  PermissionKey.peRozpis, PermissionLevel.P_MEMBER,
);
