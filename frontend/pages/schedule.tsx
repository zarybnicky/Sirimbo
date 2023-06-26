import * as React from 'react';
import { ReservationRangeDocument } from '@app/graphql/Reservation';
import { ScheduleFragment, ScheduleRangeDocument } from '@app/graphql/Schedule';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { formatWeekDay } from 'lib/format-date';
import { ScheduleItem } from 'components/ScheduleItem';
import { ReservationItem } from 'components/ReservationItem';
import { WeekPicker } from 'components/WeekPicker';
import type { NextPageWithLayout } from 'pages/_app';
import { useQuery } from 'urql';
import { endOf, startOf } from 'date-arithmetic';

const Page: NextPageWithLayout = () => {
  const [startDate, setStartDate] = React.useState(() => startOf(new Date(), 'week', 1));

  const [{ data: schedules }] = useQuery({
    query: ScheduleRangeDocument,
    variables: {
      startDate: startDate.toISOString(),
      endDate: endOf(startDate, 'week', 1).toISOString(),
    }
  });
  const [{ data: reservations }] = useQuery({
    query: ReservationRangeDocument,
    variables: {
      startDate: startDate.toISOString(),
      endDate: endOf(startDate, 'year').toISOString(),
    },
  });

  const scheduleByDay = React.useMemo(() => {
    const obj: { [date: string]: ScheduleFragment[] } = {};
    schedules?.schedulesForRange?.nodes?.forEach((item) => {
      const arr = obj[item.rDatum] || [];
      arr.push(item);
      obj[item.rDatum] = arr;
    });
    return obj;
  }, [schedules]);

  return (
    <div className="col-full-width p-4 lg:py-8">
      <WeekPicker title="Tréninky" startDate={startDate} onChange={setStartDate} />

      {!reservations?.reservationsForRange?.nodes?.length &&
        !schedules?.schedulesForRange?.nodes?.length && (
          <div className="border border-accent-6 p-2 bg-accent-1 text-accent-12 rounded-md">Žádné tréninky pro tento týden</div>
        )}

      {Object.entries(scheduleByDay).map(([date, items], i) => (
        <React.Fragment key={i}>
          <div className="text-2xl tracking-wide mt-8 mb-2">
            {formatWeekDay(new Date(date))}
          </div>

          <div className="flex justify-start flex-wrap gap-2 ml-2 pl-5 border-l-4 border-red-400">
            {items.map((item, i) => (
              <ScheduleItem key={i} item={item} />
            ))}
          </div>
        </React.Fragment>
      ))}

      {!!reservations?.reservationsForRange?.nodes?.length && (
        <>
          <div className="text-xl tracking-wide mb-2">
            Nabídky tréninků
          </div>
          <div className="flex justify-start flex-wrap gap-4 ml-2 pl-5 border-l-4 border-red-400">
            {reservations?.reservationsForRange?.nodes.map((item, i) => (
              <ReservationItem key={i} item={item} />
            ))}
          </div>
        </>
      )}

    </div>
  );
};

Page.staticTitle = "Rozpis";
Page.permissions = [PermissionKey.peRozpis, PermissionLevel.P_MEMBER];

export default Page;
